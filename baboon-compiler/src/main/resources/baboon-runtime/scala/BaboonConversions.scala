package baboon.runtime.shared {

  import scala.collection.mutable
  import scala.reflect.ClassTag

  trait Conversion {
    def typeFrom: Class[?]
    def versionFrom: String

    def typeTo: Class[?]
    def versionTo: String

    def typeId: String
  }

  trait BaboonGeneratedConversion extends Conversion {
    def convert[C](context: Option[C], conversions: AbstractBaboonConversions, from: BaboonGenerated): BaboonGenerated
  }

  abstract class AbstractConversion[From, To](
    implicit fromTag: ClassTag[From],
    toTag: ClassTag[To],
  ) extends BaboonGeneratedConversion {
    protected def doConvert[Ctx](context: Option[Ctx], conversions: AbstractBaboonConversions, from: From): To

    private def validateBaboonType(obj: Any): Unit = {
      obj match {
        case bg: BaboonGenerated =>
          val conversionTypeIsExactType = typeId == bg.baboonTypeIdentifier
          bg match {
            case bga: BaboonAdtMemberMeta =>
              val conversionTypeIsAdtType = typeId == bga.baboonAdtTypeIdentifier
              if (!conversionTypeIsAdtType && !conversionTypeIsExactType) {
                throw new IllegalArgumentException(
                  s"Provided instance is adt=${bga.baboonAdtTypeIdentifier} exact=${bg.baboonTypeIdentifier}, one of which must be $typeId"
                )
              }
            case _ =>
              if (!conversionTypeIsExactType) {
                throw new IllegalArgumentException(
                  s"Provided instance is ${bg.baboonTypeIdentifier} but must be $typeId"
                )
              }
          }
        case _ =>
      }
    }

    def convert[Ctx](context: Option[Ctx], conversions: AbstractBaboonConversions, from: From): To = {
      validateBaboonType(from)
      val result = doConvert(context, conversions, from)
      validateBaboonType(result)
      result
    }

    override def convert[Ctx](context: Option[Ctx], conversions: AbstractBaboonConversions, from: BaboonGenerated): BaboonGenerated = {
      from match {
        case f: From =>
          val res = convert[Ctx](context, conversions, f)
          res match {
            case bg: BaboonGenerated => bg
            case _ =>
              throw new IllegalArgumentException(
                s"Can't use IBaboonGeneratedConversion interface for non IBaboonGenerated return type To = ${typeTo.getName}"
              )
          }

        case _ =>
          throw new Exception(
            s"Can't use IBaboonGeneratedConversion interface when 'from' is not of type ${typeFrom.getName}"
          )
      }
    }

    override final def typeFrom: Class[?] = fromTag.runtimeClass
    override final def typeTo: Class[?]   = toTag.runtimeClass
  }

  case class ConversionKey(from: Class[?], to: Class[?])

  trait AbstractBaboonConversions {
    private val conversions     = mutable.Map.empty[ConversionKey, Conversion]
    private val typeConversions = mutable.Map.empty[Class[?], List[Conversion]]

    def register[F, T](
      conversion: AbstractConversion[F, T]
    )(implicit
      tagF: ClassTag[F],
      tagT: ClassTag[T],
    ): Unit = {
      val fromClass       = tagF.runtimeClass
      val toClass         = tagT.runtimeClass
      val key             = ConversionKey(fromClass, toClass)
      val fromConversions = typeConversions.getOrElse(fromClass, Nil)
      conversions.update(key, conversion)
      typeConversions.update(fromClass, fromConversions.appended(conversion))
    }

    def convertWithContext[C](ctx: Option[C], from: BaboonGenerated, conversion: Conversion): BaboonGenerated = {
      conversion.asInstanceOf[BaboonGeneratedConversion].convert(ctx, this, from)
    }

    def convert(from: BaboonGenerated, conversion: Conversion): BaboonGenerated = {
      conversion.asInstanceOf[BaboonGeneratedConversion].convert(None, this, from)
    }

    def convertWithContext[C, F, T](
      context: Option[C],
      from: F,
    )(implicit
      tagF: ClassTag[F],
      tagT: ClassTag[T],
    ): T = {
      val key = ConversionKey(tagF.runtimeClass, tagT.runtimeClass)
      conversions(key).asInstanceOf[AbstractConversion[F, T]].convert[C](context, this, from)
    }

    def convert[F <: BaboonGenerated, T <: BaboonGenerated](
      from: F
    )(implicit
      tagF: ClassTag[F],
      tagT: ClassTag[T],
    ): T = {
      convertWithContext[Any, F, T](None, from)(tagF, tagT)
    }

    def findConversions(value: BaboonGenerated): List[Conversion] = {
      typeConversions.get(implicitly[ClassTag[BaboonGenerated]].runtimeClass) match {
        case Some(value) => value
        case None =>
          value match {
            case m: BaboonAdtMemberMeta => typeConversions.getOrElse(m.baboonAdtType, Nil)
            case _                      => Nil
          }
      }
    }

    def versionsFrom: List[String]
    def versionTo: String
  }
}
