package io.septimalmind.baboon.util.functional

import izumi.functional.bio.Error2
import izumi.functional.bio.impl.BioEither
import izumi.functional.quasi.QuasiAsync
import izumi.fundamentals.platform.functional.Identity

import scala.collection.Factory

trait ParallelAccumulatingOps2[F[+_, +_]] {
  def parTraverseAccumErrors[ColL[_], E, A, B](
    col: IterableOnce[A]
  )(f: A => F[ColL[E], B]
  )(implicit
    buildL: Factory[E, ColL[E]],
    iterL: ColL[E] => IterableOnce[E],
  ): F[ColL[E], List[B]]
  def parTraverseAccumErrors_[ColL[_], E, A, B](
    col: IterableOnce[A]
  )(f: A => F[ColL[E], Unit]
  )(implicit
    buildL: Factory[E, ColL[E]],
    iterL: ColL[E] => IterableOnce[E],
  ): F[ColL[E], Unit]
}

object ParallelAccumulatingOpsInstances {
  object Lawless_ParallelAccumulatingOpsEither extends ParallelAccumulatingOps2[Either] {
    private val id: QuasiAsync[Identity] = QuasiAsync.quasiAsyncIdentity
    private val F: Error2[Either]        = BioEither

    override def parTraverseAccumErrors[ColL[_], E, A, B](
      col: IterableOnce[A]
    )(f: A => Either[ColL[E], B]
    )(implicit
      buildL: Factory[E, ColL[E]],
      iterL: ColL[E] => IterableOnce[E],
    ): Either[ColL[E], List[B]] = {
      F.sequenceAccumErrors(id.parTraverse(col)(f))
    }
    override def parTraverseAccumErrors_[ColL[_], E, A, B](
      col: IterableOnce[A]
    )(f: A => Either[ColL[E], Unit]
    )(implicit
      buildL: Factory[E, ColL[E]],
      iterL: ColL[E] => IterableOnce[E],
    ): Either[ColL[E], Unit] =
      F.sequenceAccumErrors_(id.parTraverse(col)(f))
  }
}
