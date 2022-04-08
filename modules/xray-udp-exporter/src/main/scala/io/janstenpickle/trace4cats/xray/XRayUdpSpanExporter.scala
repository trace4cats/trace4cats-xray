package io.janstenpickle.trace4cats.xray

import cats.Foldable
import cats.effect.kernel.Async
import io.janstenpickle.trace4cats.kernel.SpanExporter

object XRayUdpSpanExporter {
  def apply[F[_]: Async, G[_]: Foldable](host: String = "localhost", port: Int = 2000): F[SpanExporter[F, G]] = ???
}
