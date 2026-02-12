package containers

sealed trait Result[+S, +E]
object Result {
  case class Success[+S, +E](value: S) extends Result[S, E]
  case class Failure[+S, +E](error: E) extends Result[S, E]
}

class ResultServiceRt extends testpkg.pkg0.IBaboonServiceRt {
  def pure[L, R](value: R): Result[R, L] = Result.Success(value)
  def fail[L, R](error: L): Result[R, L] = Result.Failure(error)
  def leftMap[A, B, C](value: Result[B, A], f: A => C): Result[B, C] = value match {
    case Result.Failure(e) => Result.Failure(f(e))
    case Result.Success(v) => Result.Success(v)
  }
  def flatMap[A, B, C](value: Result[B, A], f: B => Result[C, A]): Result[C, A] = value match {
    case Result.Failure(e) => Result.Failure(e)
    case Result.Success(v) => f(v)
  }
}

object ResultServiceRt {
  val instance: ResultServiceRt = new ResultServiceRt()
}
