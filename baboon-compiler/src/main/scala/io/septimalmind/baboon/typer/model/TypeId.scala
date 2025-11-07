package io.septimalmind.baboon.typer.model

sealed trait TypeId {
  def name: TypeName
}

object TypeId {
  sealed trait Builtin extends TypeId

  sealed trait Scalar extends TypeId

  case class BuiltinScalar(name: TypeName) extends Builtin with Scalar {
    override def toString: String = s"#${name.name}"
  }

  case class BuiltinCollection(name: TypeName) extends Builtin {
    override def toString: String = s"#${name.name}"
  }

  case class User(pkg: Pkg, owner: Owner, name: TypeName) extends TypeId with Scalar {
    override def toString: String = {
      s"$pkg/$owner#${name.name}"
    }

    def render: String = {
      (pkg.path ++ owner.asPseudoPkg ++ Seq(name.name)).mkString(".")
    }
  }

  object Builtins {

    final val bit = BuiltinScalar(TypeName("bit"))

    final val i08 = BuiltinScalar(TypeName("i08"))
    final val i16 = BuiltinScalar(TypeName("i16"))
    final val i32 = BuiltinScalar(TypeName("i32"))
    final val i64 = BuiltinScalar(TypeName("i64"))

    final val u08 = BuiltinScalar(TypeName("u08"))
    final val u16 = BuiltinScalar(TypeName("u16"))
    final val u32 = BuiltinScalar(TypeName("u32"))
    final val u64 = BuiltinScalar(TypeName("u64"))

    final val f32  = BuiltinScalar(TypeName("f32"))
    final val f64  = BuiltinScalar(TypeName("f64"))
    final val f128 = BuiltinScalar(TypeName("f128"))

    final val str = BuiltinScalar(TypeName("str"))
    final val uid = BuiltinScalar(TypeName("uid"))
    final val tsu = BuiltinScalar(TypeName("tsu"))
    final val tso = BuiltinScalar(TypeName("tso"))

    final val map = BuiltinCollection(TypeName("map"))
    final val opt = BuiltinCollection(TypeName("opt"))
    final val lst = BuiltinCollection(TypeName("lst"))
    final val set = BuiltinCollection(TypeName("set"))

  }

  sealed trait ComparatorType

  object ComparatorType {
    sealed trait Basic extends ComparatorType

    case object Direct extends Basic

    case object ObjectEquals extends Basic

    sealed trait Complex extends ComparatorType

    case class OptionEquals(subComparator: ComparatorType) extends Complex

    case class SeqEquals(subComparator: ComparatorType) extends Complex

    case class SetEquals(subComparator: ComparatorType) extends Complex

    case class MapEquals(keyComparator: ComparatorType, valComparator: ComparatorType) extends Complex
  }

}
