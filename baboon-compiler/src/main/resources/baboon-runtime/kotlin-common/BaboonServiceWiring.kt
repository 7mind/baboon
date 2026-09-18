package baboon.runtime.shared

data class BaboonMethodId(val serviceName: String, val methodName: String)

sealed class BaboonWiringError {
    data class NoMatchingMethod(val method: BaboonMethodId) : BaboonWiringError()
    data class NoMatchingService(val method: BaboonMethodId) : BaboonWiringError()
    data class DuplicateService(val serviceName: String) : BaboonWiringError()
    data class DecoderFailed(val method: BaboonMethodId, val exception: Throwable) : BaboonWiringError()
    data class EncoderFailed(val method: BaboonMethodId, val exception: Throwable) : BaboonWiringError()
    data class CallFailed(val method: BaboonMethodId, val domainError: Any) : BaboonWiringError()
}

class BaboonWiringException(val error: BaboonWiringError) : RuntimeException(error.toString())

// --- Service muxers ---
//
// Cross-domain composable dispatch. A muxer holds a set of services from any
// model(s) and routes an `(method, data, ctx)` call to the right one by
// `method.serviceName`. The R type parameter encodes the return shape so the
// same class supports both sync invokers (R = String / ByteArray) and
// container-wrapped invokers (R = Either<BaboonWiringError, String>, etc.).
// The per-service wrapper classes emitted alongside `invokeJson` / `invokeUeba`
// carry the matching parameterisation.

interface IBaboonJsonService<R> {
    val serviceName: String
    fun invoke(method: BaboonMethodId, data: String, ctx: BaboonCodecContext): R
}

interface IBaboonUebaService<R> {
    val serviceName: String
    fun invoke(method: BaboonMethodId, data: ByteArray, ctx: BaboonCodecContext): R
}

class JsonMuxer<R>(vararg services: IBaboonJsonService<R>) {
    private val table: MutableMap<String, IBaboonJsonService<R>> = mutableMapOf()

    init {
        for (s in services) register(s)
    }

    fun register(service: IBaboonJsonService<R>) {
        if (table.containsKey(service.serviceName)) {
            throw BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    fun invoke(method: BaboonMethodId, data: String, ctx: BaboonCodecContext): R {
        val service = table[method.serviceName]
            ?: throw BaboonWiringException(BaboonWiringError.NoMatchingService(method))
        return service.invoke(method, data, ctx)
    }

    fun serviceNames(): Set<String> = table.keys.toSet()
}

class UebaMuxer<R>(vararg services: IBaboonUebaService<R>) {
    private val table: MutableMap<String, IBaboonUebaService<R>> = mutableMapOf()

    init {
        for (s in services) register(s)
    }

    fun register(service: IBaboonUebaService<R>) {
        if (table.containsKey(service.serviceName)) {
            throw BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    fun invoke(method: BaboonMethodId, data: ByteArray, ctx: BaboonCodecContext): R {
        val service = table[method.serviceName]
            ?: throw BaboonWiringException(BaboonWiringError.NoMatchingService(method))
        return service.invoke(method, data, ctx)
    }

    fun serviceNames(): Set<String> = table.keys.toSet()
}

// --- Context-carrying service variants ---
//
// Emitted when a service.context mode (`abstract` or `type`) is active. The
// abstract/concrete service context `Ctx` is supplied per-invocation (alongside
// the codec context) rather than baked into the wrapper, so callers thread a
// fresh context through each dispatch. The context-free interfaces above are
// left untouched so `--service-context-mode none` output (and the
// service-acceptance matrix) is byte-identical.

interface IBaboonJsonServiceCtx<Ctx, R> {
    val serviceName: String
    fun invoke(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): R
}

interface IBaboonUebaServiceCtx<Ctx, R> {
    val serviceName: String
    fun invoke(method: BaboonMethodId, data: ByteArray, ctx: Ctx, codecCtx: BaboonCodecContext): R
}

class JsonMuxerCtx<Ctx, R>(vararg services: IBaboonJsonServiceCtx<Ctx, R>) {
    private val table: MutableMap<String, IBaboonJsonServiceCtx<Ctx, R>> = mutableMapOf()

    init {
        for (s in services) register(s)
    }

    fun register(service: IBaboonJsonServiceCtx<Ctx, R>) {
        if (table.containsKey(service.serviceName)) {
            throw BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    fun invoke(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): R {
        val service = table[method.serviceName]
            ?: throw BaboonWiringException(BaboonWiringError.NoMatchingService(method))
        return service.invoke(method, data, ctx, codecCtx)
    }

    fun serviceNames(): Set<String> = table.keys.toSet()
}

class UebaMuxerCtx<Ctx, R>(vararg services: IBaboonUebaServiceCtx<Ctx, R>) {
    private val table: MutableMap<String, IBaboonUebaServiceCtx<Ctx, R>> = mutableMapOf()

    init {
        for (s in services) register(s)
    }

    fun register(service: IBaboonUebaServiceCtx<Ctx, R>) {
        if (table.containsKey(service.serviceName)) {
            throw BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    fun invoke(method: BaboonMethodId, data: ByteArray, ctx: Ctx, codecCtx: BaboonCodecContext): R {
        val service = table[method.serviceName]
            ?: throw BaboonWiringException(BaboonWiringError.NoMatchingService(method))
        return service.invoke(method, data, ctx, codecCtx)
    }

    fun serviceNames(): Set<String> = table.keys.toSet()
}
