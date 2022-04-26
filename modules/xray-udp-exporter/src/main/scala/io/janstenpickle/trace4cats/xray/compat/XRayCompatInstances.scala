package io.janstenpickle.trace4cats.xray.compat

import cats.Apply
import cats.effect.kernel.{Clock, Sync}
import cats.effect.std.Random
import io.janstenpickle.trace4cats.model.TraceId

trait XRayCompatInstances0 extends XRayCompatInstances1 {
  implicit def fromRandomClockXRayTraceIdGen[F[_]: Apply: Random: Clock]: TraceId.Gen[F] =
    explicits.xRayTraceIdGen[F]
}

trait XRayCompatInstances1 {
  implicit def fromSyncXRayTraceIdGen[F[_]: Sync]: TraceId.Gen[F] = {
    implicit val rnd: Random[F] = Random.javaUtilConcurrentThreadLocalRandom[F]
    explicits.xRayTraceIdGen[F]
  }
}
