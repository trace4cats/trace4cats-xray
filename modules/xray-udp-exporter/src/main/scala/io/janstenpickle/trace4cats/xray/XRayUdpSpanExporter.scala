package io.janstenpickle.trace4cats.xray

import cats.Foldable
import cats.effect.kernel.Async
import cats.syntax.either._
import cats.syntax.functor._
import io.janstenpickle.trace4cats.`export`.HttpSpanExporter
import io.janstenpickle.trace4cats.kernel.SpanExporter
import org.http4s.Uri
import org.http4s.client.Client

object XRayUdpSpanExporter {
  def apply[F[_]: Async, G[_]: Foldable](host: String = "localhost", port: Int = 2000): F[SpanExporter[F, G]] = ???
}
