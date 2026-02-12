package containers

sealed trait Outcome[+T]
object Outcome {
  case class Success[+T](value: T) extends Outcome[T]
  case class Failure[+T](error: Any) extends Outcome[T]
}

class OutcomeServiceRt extends testpkg.pkg0.IBaboonServiceRt {
  def pure[L, R](value: R): Outcome[R] = Outcome.Success(value)
  def fail[L, R](error: L): Outcome[R] = Outcome.Failure(error)
  def leftMap[A, B, C](value: Outcome[B], f: A => C): Outcome[B] = value match {
    case Outcome.Failure(e) => Outcome.Failure(f(e.asInstanceOf[A]))
    case s: Outcome.Success[B] => s
  }
  def flatMap[A, B, C](value: Outcome[B], f: B => Outcome[C]): Outcome[C] = value match {
    case Outcome.Failure(e) => Outcome.Failure(e)
    case Outcome.Success(v) => f(v)
  }
}

object OutcomeServiceRt {
  val instance: OutcomeServiceRt = new OutcomeServiceRt()
}
