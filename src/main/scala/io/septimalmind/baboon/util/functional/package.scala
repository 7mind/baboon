package io.septimalmind.baboon.util

import izumi.functional.bio.Functor2

import scala.annotation.unused

package object functional {
  @inline implicit final def ParallelAccumulatingOps2[F[+_, +_]](@unused self: Functor2[F])(implicit Parallel: ParallelAccumulatingOps2[F]): Parallel.type = Parallel
}
