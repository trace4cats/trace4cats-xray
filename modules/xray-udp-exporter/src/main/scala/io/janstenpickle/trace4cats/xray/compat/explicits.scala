package io.janstenpickle.trace4cats.xray.compat

import cats.{Apply, Show}
import cats.effect.kernel.Clock
import cats.effect.std.Random
import cats.syntax.apply._
import io.janstenpickle.trace4cats.model.TraceId
import org.apache.commons.codec.binary.Hex

import java.nio.ByteBuffer

object explicits {

  /** Generates an X-Ray `trace_id` based on `Clock[F]` and `Random[F]`:
    *   1. the first 4 bytes are epoch time;
    *   1. the rest bytes are random.
    */
  def xRayTraceIdGen[F[_]: Apply: Random: Clock]: TraceId.Gen[F] = TraceId.Gen.from {
    val timeSize = Integer.BYTES
    val rndSize = TraceId.size - timeSize
    (Clock[F].realTime, Random[F].nextBytes(rndSize)).mapN { (t, r) =>
      val bytes = ByteBuffer.allocate(TraceId.size).putInt(t.toSeconds.toInt).put(r).array
      TraceId.unsafe(bytes)
    }
  }

  /** Encodes an X-Ray `trace_id` as 3 hyphen-separated tokens:
    *   1. the version, that is, 1;
    *   1. the time of the original request, in Unix epoch time, in 8 hexadecimal digits;
    *   1. a 96-bit identifier for the trace, globally unique, in 24 hexadecimal digits.
    *
    * See documentation under
    * [[https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-fields "Required Segment Fields"]].
    */
  val xRayTraceIdExportShow: Show[TraceId] = Show.show { traceId =>
    val timeSize = Integer.BYTES
    val rndSize = TraceId.size - timeSize
    val bytes = traceId.value
    val t = new String(Hex.encodeHex(bytes, 0, timeSize, true))
    val r = new String(Hex.encodeHex(bytes, timeSize, rndSize, true))
    s"1-$t-$r"
  }

}
