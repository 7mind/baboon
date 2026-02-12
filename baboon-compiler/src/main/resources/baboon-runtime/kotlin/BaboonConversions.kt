package baboon.runtime.shared

interface Conversion {
    val typeFrom: Class<*>
    val versionFrom: String
    val typeTo: Class<*>
    val versionTo: String
    val typeId: String
}

interface BaboonGeneratedConversion : Conversion {
    fun <C> convert(context: C?, conversions: AbstractBaboonConversions, from: BaboonGenerated): BaboonGenerated
}

abstract class AbstractConversion<From : Any, To : Any>(
    private val fromClass: Class<From>,
    private val toClass: Class<To>,
) : BaboonGeneratedConversion {
    protected abstract fun <Ctx> doConvert(context: Ctx?, conversions: AbstractBaboonConversions, from: From): To

    private fun validateBaboonType(obj: Any) {
        when (obj) {
            is BaboonGenerated -> {
                val conversionTypeIsExactType = typeId == obj.baboonTypeIdentifier
                when (obj) {
                    is BaboonAdtMemberMeta -> {
                        val conversionTypeIsAdtType = typeId == obj.baboonAdtTypeIdentifier
                        if (!conversionTypeIsAdtType && !conversionTypeIsExactType) {
                            throw IllegalArgumentException(
                                "Provided instance is adt=${obj.baboonAdtTypeIdentifier} exact=${obj.baboonTypeIdentifier}, one of which must be $typeId"
                            )
                        }
                    }
                    else -> {
                        if (!conversionTypeIsExactType) {
                            throw IllegalArgumentException(
                                "Provided instance is ${obj.baboonTypeIdentifier} but must be $typeId"
                            )
                        }
                    }
                }
            }
        }
    }

    fun <Ctx> convert(context: Ctx?, conversions: AbstractBaboonConversions, from: From): To {
        validateBaboonType(from)
        val result = doConvert(context, conversions, from)
        validateBaboonType(result)
        return result
    }

    @Suppress("UNCHECKED_CAST")
    override fun <C> convert(context: C?, conversions: AbstractBaboonConversions, from: BaboonGenerated): BaboonGenerated {
        val f = fromClass.cast(from) as From
        val res = convert(context, conversions, f)
        return res as BaboonGenerated
    }

    override val typeFrom: Class<*> get() = fromClass
    override val typeTo: Class<*> get() = toClass
}

data class ConversionKey(val from: Class<*>, val to: Class<*>)

abstract class AbstractBaboonConversions {
    private val conversions = mutableMapOf<ConversionKey, Conversion>()
    private val typeConversions = mutableMapOf<Class<*>, MutableList<Conversion>>()

    fun <F : Any, T : Any> register(conversion: AbstractConversion<F, T>) {
        val fromClass = conversion.typeFrom
        val toClass = conversion.typeTo
        val key = ConversionKey(fromClass, toClass)
        conversions[key] = conversion
        typeConversions.getOrPut(fromClass) { mutableListOf() }.add(conversion)
    }

    fun <C> convertWithContext(ctx: C?, from: BaboonGenerated, conversion: Conversion): BaboonGenerated {
        @Suppress("UNCHECKED_CAST")
        return (conversion as BaboonGeneratedConversion).convert(ctx, this, from)
    }

    fun <C> convertWithContext(ctx: C?, from: BaboonGenerated, fromClass: Class<*>, toClass: Class<*>): BaboonGenerated {
        val key = ConversionKey(fromClass, toClass)
        val conv = conversions[key] ?: throw NoSuchElementException("No conversion from $fromClass to $toClass")
        @Suppress("UNCHECKED_CAST")
        return (conv as BaboonGeneratedConversion).convert(ctx, this, from)
    }

    fun convert(from: BaboonGenerated, conversion: Conversion): BaboonGenerated {
        return convertWithContext<Any>(null, from, conversion)
    }

    fun findConversions(value: BaboonGenerated): List<Conversion> {
        val fromType = value::class.java
        return typeConversions[fromType]
            ?: when (value) {
                is BaboonAdtMemberMeta -> typeConversions[value.baboonAdtType] ?: emptyList()
                else -> emptyList()
            }
    }

    abstract val versionsFrom: List<String>
    abstract val versionTo: String
}
