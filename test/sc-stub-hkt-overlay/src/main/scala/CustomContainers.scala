package custom

sealed trait MyBi[+L, +R]
object MyBi {
  case class Good[+L, +R](value: R) extends MyBi[L, R]
  case class Bad[+L, +R](error: L) extends MyBi[L, R]
}

class MyBiServiceRt extends testpkg.pkg0.IBaboonServiceRt[MyBi] {
  def pure[L, R](value: R): MyBi[L, R] = MyBi.Good(value)
  def fail[L, R](error: L): MyBi[L, R] = MyBi.Bad(error)
  def leftMap[A, B, C](value: MyBi[A, B], f: A => C): MyBi[C, B] = value match {
    case MyBi.Bad(e) => MyBi.Bad(f(e))
    case MyBi.Good(v) => MyBi.Good(v)
  }
  def flatMap[A, B, C](value: MyBi[A, B], f: B => MyBi[A, C]): MyBi[A, C] = value match {
    case MyBi.Bad(e) => MyBi.Bad(e)
    case MyBi.Good(v) => f(v)
  }
}

object MyBiServiceRt {
  val instance: MyBiServiceRt = new MyBiServiceRt()
}
