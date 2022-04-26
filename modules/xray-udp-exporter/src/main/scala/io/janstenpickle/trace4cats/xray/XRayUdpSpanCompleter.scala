package io.janstenpickle.trace4cats.xray

import cats.effect.kernel.{Async, Resource}
import cats.effect.std.Random
import com.comcast.ip4s._
import fs2.Chunk
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import io.janstenpickle.trace4cats.`export`.{CompleterConfig, QueuedSpanCompleter}
import io.janstenpickle.trace4cats.kernel.SpanCompleter
import io.janstenpickle.trace4cats.model.TraceProcess

object XRayUdpSpanCompleter {
  def apply[F[_]: Async: Random](
    process: TraceProcess,
    host: Host = XRayUdpSpanExporter.defaultHost,
    port: Port = XRayUdpSpanExporter.defaultPort,
    config: CompleterConfig = CompleterConfig()
  ): Resource[F, SpanCompleter[F]] =
    Resource.eval(Slf4jLogger.create[F]).flatMap { implicit logger: Logger[F] =>
      XRayUdpSpanExporter[F, Chunk](Some(process), host, port)
        .flatMap(QueuedSpanCompleter[F](process, _, config))
    }
}
