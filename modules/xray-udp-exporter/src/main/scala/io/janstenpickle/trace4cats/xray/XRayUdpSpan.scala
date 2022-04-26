package io.janstenpickle.trace4cats.xray

import cats.data.NonEmptyList
import cats.effect.kernel.Ref
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.{Applicative, Eval, Monad}
import fs2.Chunk
import io.circe.syntax._
import io.circe.{Encoder, Json, JsonObject, Printer}
import io.janstenpickle.trace4cats.`export`.SemanticTags
import io.janstenpickle.trace4cats.model._
import io.janstenpickle.trace4cats.xray.compat.explicits._

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.util.matching.Regex

private[xray] object XRayUdpSpan {

  /** Span names can contain Unicode letters, numbers, and whitespace, and a limited set of symbols.
    *
    * See the `name` documentation under "Required Segment Fields"
    * https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-fields
    */
  private val nameIllegalCharRegex: Regex = """[^\p{L}0-9 _.:/%&#=\\+\-@]""".r

  /** Annotation keys must be alphanumeric in order to work with filters. Underscore is allowed. Other symbols and
    * whitespace are not allowed.
    *
    * See the documentation under "Annotations"
    * https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-annotations
    */
  private val keyIllegalCharRegex: Regex = """[^A-Za-z0-9_]""".r

  private implicit val attributeValueEncoder: Encoder[AttributeValue] = Encoder.instance {
    case AttributeValue.StringValue(value) => Json.fromString(value.value)
    case AttributeValue.BooleanValue(value) => Json.fromBoolean(value.value)
    case AttributeValue.LongValue(value) => Json.fromLong(value.value)
    case AttributeValue.DoubleValue(value) => Json.fromDoubleOrString(value.value)
    case value: AttributeValue.AttributeList => Json.fromString(value.show)
  }

  private val statusTags = SemanticTags.statusTags("span.")

  /** The time the segment was created/closed, in floating point seconds in epoch time. Microsecond resolution is
    * recommended when available.
    *
    * See the `start_time` and `end_time` documentation under "Required Segment Fields"
    * https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-fields
    */
  private def toEpochSeconds(i: Instant): Double =
    ChronoUnit.MICROS.between(Instant.EPOCH, i).toDouble / 1000_000

  private def spanStatusFaultJson[F[_]: Applicative: XRayExceptionId.Gen](status: SpanStatus): F[JsonObject] =
    XRayExceptionId.gen[F].map { id =>
      JsonObject(
        "fault" := true,
        "cause" := Json.obj(
          "exceptions" := Json.arr(
            Json.obj(
              "id" := id.show,
              "message" := Some(status).collect { case SpanStatus.Internal(msg) => msg },
              "type" := status.entryName
            )
          )
        )
      )
    }

  private def spanStatusJson[F[_]: Applicative: XRayExceptionId.Gen](status: SpanStatus): F[JsonObject] =
    status match {
      case SpanStatus.Ok => JsonObject.empty.pure[F]
      case _ => spanStatusFaultJson[F](status)
    }

  private def jsonToChunk(j: Json): Chunk[Byte] =
    Chunk.byteBuffer(Printer.noSpaces.printToByteBuffer(j))

  private def json[F[_]: Monad: XRayExceptionId.Gen](
    process: Option[TraceProcess],
    span: CompletedSpan,
    childSpans: Ref[F, Map[TraceId, NonEmptyList[CompletedSpan]]]
  ): F[JsonObject] = {
    val (badName: Option[String], goodName: String) =
      if (nameIllegalCharRegex.findFirstMatchIn(span.name).isDefined)
        (Some(span.name), nameIllegalCharRegex.replaceAllIn(span.name, "_"))
      else
        (None, span.name)

    val (badKeys: Map[String, AttributeValue], goodKeys: Map[String, AttributeValue]) =
      (process.fold(Map.empty[String, AttributeValue])(_.attributes) ++ span.allAttributes ++ statusTags(
        span.status
      ) ++ SemanticTags.kindTags(span.kind)).partition { case (k, _) =>
        keyIllegalCharRegex.findFirstMatchIn(k).isDefined
      }

    val fixedAnnotations = badKeys.map { case (k, v) =>
      keyIllegalCharRegex.replaceAllIn(k, "_") -> v
    }
    val allAnnotations: Map[String, AttributeValue] =
      goodKeys ++
        badName.map(n => "malformed_name" -> AttributeValue.StringValue(Eval.now(n))) ++
        (if (badKeys.nonEmpty)
           Map("malformed_keys" -> AttributeValue.StringValue(Eval.now(badKeys.keys.mkString(","))))
         else
           Map.empty) ++
        fixedAnnotations

    (
      spanStatusJson[F](span.status),
      childSpans
        .modify(m => (m - span.context.traceId, m.get(span.context.traceId)))
        .flatMap(_.traverse(_.traverse(json[F](process, _, childSpans))))
    ).mapN { (statusJson, children) =>
      JsonObject(
        "name" := goodName,
        "id" := span.context.spanId.show,
        "trace_id" := xRayTraceIdExportShow.show(span.context.traceId),
        "parent_id" := span.context.parent.map(_.spanId.show),
        "start_time" := toEpochSeconds(span.start),
        "end_time" := toEpochSeconds(span.end),
        "annotations" := allAnnotations.asJson,
        "subsegments" := children
      ).deepMerge(statusJson)
    }
  }

  val jsonHeader: Chunk[Byte] = jsonToChunk(Json.obj("format" := "json", "version" := 1))

  def jsonChunk[F[_]: Monad: XRayExceptionId.Gen](
    process: Option[TraceProcess],
    span: CompletedSpan,
    childSpans: Ref[F, Map[TraceId, NonEmptyList[CompletedSpan]]]
  ): F[Chunk[Byte]] =
    json[F](process, span, childSpans).map { jsonObj =>
      jsonToChunk(Json.fromJsonObject(jsonObj).deepDropNullValues)
    }

}
