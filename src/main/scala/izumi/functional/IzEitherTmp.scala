package izumi.functional

object IzEitherTmp {
  @inline implicit final def EitherObjectExt(e: Either.type): EitherExt =
    new EitherExt(e)

  final class EitherExt(val e: Either.type) extends AnyVal {
    def failWhen[A](cond: Boolean)(fun: => A): Either[A, Unit] = {
      if (cond) {
        Left(fun)
      } else {
        Right(())
      }
    }
  }
}
