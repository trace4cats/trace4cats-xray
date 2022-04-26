package io.janstenpickle.trace4cats.xray

import cats.effect.kernel.Sync
import cats.effect.std.Random
import cats.syntax.functor._
import cats.syntax.show._
import cats.{Eq, Functor, Show}
import org.apache.commons.codec.binary.Hex

import scala.util.Try

case class XRayExceptionId private (value: Array[Byte]) extends AnyVal {
  override def toString: String = show"XRayExceptionId($this)"
}

object XRayExceptionId {
  val size = 8

  trait Gen[F[_]] {
    def gen: F[XRayExceptionId]
  }

  object Gen extends GenInstances0 {
    def apply[F[_]](implicit ev: Gen[F]): Gen[F] = ev

    def from[F[_]](f: F[XRayExceptionId]): Gen[F] = new Gen[F] {
      def gen: F[XRayExceptionId] = f
    }
  }

  trait GenInstances0 extends GenInstances1 {
    implicit def fromRandomTraceIdGen[F[_]: Functor: Random]: Gen[F] =
      Gen.from(Random[F].nextBytes(size).map(XRayExceptionId.unsafe))
  }

  trait GenInstances1 { this: GenInstances0 =>
    implicit def threadLocalRandomTraceIdGen[F[_]: Sync]: Gen[F] = {
      implicit val rnd: Random[F] = Random.javaUtilConcurrentThreadLocalRandom[F]
      fromRandomTraceIdGen[F]
    }
  }

  def gen[F[_]: Gen]: F[XRayExceptionId] = Gen[F].gen

  def fromHexString(hex: String): Option[XRayExceptionId] =
    Try(Hex.decodeHex(hex)).toOption.flatMap(apply)

  def apply(array: Array[Byte]): Option[XRayExceptionId] =
    if (array.length == size) Some(new XRayExceptionId(array)) else None

  def unsafe(array: Array[Byte]): XRayExceptionId =
    apply(array).getOrElse(
      throw new IllegalArgumentException(s"Expected a byte-array of size $size, got ${array.length}")
    )

  val invalid: XRayExceptionId = new XRayExceptionId(Array.fill(size)(0))

  implicit val show: Show[XRayExceptionId] =
    Show.show(tid => Hex.encodeHexString(tid.value))

  implicit val eq: Eq[XRayExceptionId] = Eq.by(_.show)
}
