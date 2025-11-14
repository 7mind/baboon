package io.septimalmind.baboon.typer

import io.septimalmind.baboon.typer.model.TypeId.{Builtins, ComparatorType}
import io.septimalmind.baboon.typer.model.{TypeId, TypeRef}

trait TypeInfo {
  def hasDefaultValue(id: TypeRef.Constructor): Boolean
  def isCompatibleChange(ro: TypeRef, rn: TypeRef): Boolean
  def isPrecisionExpansion(o: TypeId, n: TypeId): Boolean
  def canBeWrappedIntoCollection(o: TypeRef.Scalar, n: TypeRef.Constructor): Boolean
  def canChangeCollectionType(o: TypeRef.Constructor, n: TypeRef.Constructor): Boolean
  def isBultinScalar(id: TypeId): Boolean
  def isBultinCollection(id: TypeId): Boolean
  def comparator(ref: TypeRef): ComparatorType
  def allBuiltins: Set[TypeId.Builtin]
}

object TypeInfo {
  sealed trait NumberType
  object NumberType {
    case object Integer extends NumberType
    case object UInteger extends NumberType
    case object Float extends NumberType
  }

  sealed trait NumberInfo
  object NumberInfo {
    case class Number(kind: NumberType, size: Int) extends NumberInfo
    case object Nan extends NumberInfo
  }

  class TypeInfoImpl extends TypeInfo {
    import io.septimalmind.baboon.typer.model.TypeId.Builtins.*

    private final val integers   = Set(i08, i16, i32, i64, u08, u16, u32, u64)
    private final val floats     = Set(f32, f64, f128)
    private final val timestamps = Set(tsu, tso)
    private final val stringy    = Set(uid)
    private final val varlens    = Set(str)

    private final val seqCollections      = Set(lst, set)
    private final val iterableCollections = Set(map) ++ seqCollections
    private final val collections         = Set(opt) ++ iterableCollections

    private final val scalars = integers ++ floats ++ varlens ++ stringy ++ timestamps ++ Set(
      bit
    )
    private final val all: Set[TypeId.Builtin] = scalars ++ collections

    private final val collIds     = collections.toSet[TypeId]
    private final val seqColls    = seqCollections.toSet[TypeId]
    private final val safeSources = seqColls ++ Set(TypeId.Builtins.opt)
    private val scalarsSet        = scalars.toSet[TypeId]

    override def allBuiltins: Set[TypeId.Builtin] = all

    def comparator(ref: TypeRef): ComparatorType = {
      ref match {
        case TypeRef.Scalar(id) =>
          if (scalarsSet.contains(id)) {
            ComparatorType.Direct
          } else {
            ComparatorType.ObjectEquals
          }
        case c: TypeRef.Constructor =>
          val arg1 = c.args.head

          c.id match {
            case TypeId.Builtins.opt =>
              comparator(arg1) match {
                case ComparatorType.Direct => ComparatorType.Direct
                case out                   => ComparatorType.OptionEquals(out)
              }
            case TypeId.Builtins.set =>
              ComparatorType.SetEquals(comparator(arg1))

            case TypeId.Builtins.map =>
              ComparatorType.MapEquals(comparator(arg1), comparator(c.args.last))
            case TypeId.Builtins.lst =>
              ComparatorType.SeqEquals(comparator(arg1))
            case _ =>
              ComparatorType.ObjectEquals
          }
      }
    }

    override def isBultinScalar(id: TypeId): Boolean = {
      scalarsSet.contains(id)
    }

    override def isBultinCollection(id: TypeId): Boolean = {
      collIds.contains(id)
    }

    def hasDefaultValue(id: TypeRef.Constructor): Boolean =
      collIds.contains(id.id)

    def canBeWrappedIntoCollection(o: TypeRef.Scalar, n: TypeRef.Constructor): Boolean = {
      safeSources.contains(n.id) && n.args.length == 1 && isCompatibleChange(o, n.args.head)
    }

    def canChangeCollectionType(o: TypeRef.Constructor, n: TypeRef.Constructor): Boolean = {
      // we can safely change collection types between list <-> set, opt -> (list | set)
      val isSwap = safeSources.contains(o.id) && seqColls.contains(n.id)

      val isSimpleSwap = o.args == n.args && isSwap

      val isPrecex = o.args.length == n.args.length && o.args.toSeq
        .zip(n.args.toSeq)
        .forall {
          case (o, n) => isPrecisionExpansion(o.id, n.id)
        }
      val compatibleUpgrage = (o.args.length == n.args.length) && (o.args.toList.zip(n.args).forall {
        case (ao, an) =>
          val r = isCompatibleChange(ao, an)
          r
      })

      val isSimplePrecex = (o.id == n.id) && isPrecex
      val isSwapPrecex   = isSwap && isPrecex

      val isSimpleUpgrade = (o.id == n.id) && compatibleUpgrage
      val isSwapUpgrade   = isSwap && compatibleUpgrage

      val r = isSimpleSwap || isSimplePrecex || isSwapPrecex || isSimpleUpgrade || isSwapUpgrade
      r
    }

    def isCompatibleChange(ro: TypeRef, rn: TypeRef): Boolean = {
      if (ro == rn) {
        true
      } else {
        (ro, rn) match {
          case (_: TypeRef.Constructor, _: TypeRef.Scalar) =>
            false
          case (o: TypeRef.Scalar, n: TypeRef.Scalar) =>
            // TODO: precex in collections
            isPrecisionExpansion(o.id, n.id)
          case (o: TypeRef.Scalar, n: TypeRef.Constructor) =>
            canBeWrappedIntoCollection(o, n)
          case (o: TypeRef.Constructor, n: TypeRef.Constructor) =>
            canChangeCollectionType(o, n)
        }
      }

    }

    def isPrecisionExpansion(o: TypeId, n: TypeId): Boolean = {
      (classifyNumber(o), classifyNumber(n)) match {
        case (NumberInfo.Number(oldKind, oldSize), NumberInfo.Number(newKind, newSize)) =>
          oldKind == newKind && oldSize < newSize
        case _ =>
          false
      }
    }

    private def classifyNumber(typeId: TypeId): NumberInfo = {
      typeId match {
        case Builtins.i08  => NumberInfo.Number(NumberType.Integer, 8)
        case Builtins.i16  => NumberInfo.Number(NumberType.Integer, 16)
        case Builtins.i32  => NumberInfo.Number(NumberType.Integer, 32)
        case Builtins.i64  => NumberInfo.Number(NumberType.Integer, 64)
        case Builtins.u08  => NumberInfo.Number(NumberType.UInteger, 8)
        case Builtins.u16  => NumberInfo.Number(NumberType.UInteger, 16)
        case Builtins.u32  => NumberInfo.Number(NumberType.UInteger, 32)
        case Builtins.u64  => NumberInfo.Number(NumberType.UInteger, 64)
        case Builtins.f32  => NumberInfo.Number(NumberType.Float, 32)
        case Builtins.f64  => NumberInfo.Number(NumberType.Float, 64)
        case Builtins.f128 => NumberInfo.Number(NumberType.Float, 128)
        case _             => NumberInfo.Nan
      }
    }

  }
}
