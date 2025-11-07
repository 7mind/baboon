package io.septimalmind.baboon

sealed trait RuntimeGenOpt

object RuntimeGenOpt {
  case object Only extends RuntimeGenOpt

  case object With extends RuntimeGenOpt

  case object Without extends RuntimeGenOpt
}
