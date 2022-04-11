package io.janstenpickle.trace4cats.xray

import cats.Foldable
import cats.data.NonEmptyList
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.effect.std.Random
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import com.comcast.ip4s._
import fs2.Chunk
import fs2.io.net.{Datagram, Network}
import io.janstenpickle.trace4cats.kernel.SpanExporter
import io.janstenpickle.trace4cats.model.{Batch, CompletedSpan, TraceId, TraceProcess}
import java.nio.charset.StandardCharsets

object XRayUdpSpanExporter {
  val defaultHost: Host = Option(System.getenv("XRAY_AGENT_HOST"))
    .flatMap(Host.fromString)
    .getOrElse(ip"127.0.0.1")

  val defaultPort: Port = Option(System.getenv("XRAY_AGENT_PORT"))
    .flatMap(Port.fromString)
    .getOrElse(port"2000")

  private val newLine = Chunk.array("\n".getBytes(StandardCharsets.UTF_8))

  private def resolveIp[F[_]: Async](host: Host, ipRef: Deferred[F, IpAddress]): F[IpAddress] =
    ipRef.tryGet.flatMap {
      case Some(ip) => ip.pure[F]
      case None => host.resolve[F].flatTap(ipRef.complete)
    }

  def apply[F[_]: Async: Random, G[_]: Foldable](
    process: Option[TraceProcess],
    host: Host = defaultHost,
    port: Port = defaultPort
  ): Resource[F, SpanExporter[F, G]] =
    /** Initialize a `Ref` to hold a map of `TraceId` to `NonEmptyList[CompletedSpan]`. Spans with a parent will be
      * accumulated in this map and eventually exported along with their parent.
      */
    (Network[F].openDatagramSocket(), Resource.eval(Ref.of[F, Map[TraceId, NonEmptyList[CompletedSpan]]](Map.empty)))
      .mapN { (socket, childSpans) =>
        new SpanExporter[F, G] {
          def sendSpan(ipRef: Deferred[F, IpAddress], span: CompletedSpan): F[Unit] =
            (resolveIp[F](host, ipRef), XRayUdpSpan.jsonChunk[F](process, span, childSpans)).tupled
              .flatMap { case (ip, chunk) =>
                socket.write(Datagram(SocketAddress(ip, port), XRayUdpSpan.jsonHeader ++ newLine ++ chunk))
              }

          override def exportBatch(batch: Batch[G]): F[Unit] =
            /** Initialize a `Deferred` to hold the target `IpAddress` so we only need to resolve it at most once for
              * each call to `exportBatch`
              */
            Deferred[F, IpAddress].flatMap { ipRef =>
              batch.spans.traverse_ { span =>
                span.context.parent match {
                  /** If the span has a parent, just store it in our `childSpans` reference */
                  case Some(_) =>
                    childSpans.update { m =>
                      m.updated(
                        span.context.traceId,
                        m.get(span.context.traceId) match {
                          case Some(children) => span :: children
                          case None => NonEmptyList.one(span)
                        }
                      )
                    }

                  /** If the span doesn't have a parent, export it and all of its children */
                  case None =>
                    sendSpan(ipRef, span)
                }
              }
            }
        }
      }
}
