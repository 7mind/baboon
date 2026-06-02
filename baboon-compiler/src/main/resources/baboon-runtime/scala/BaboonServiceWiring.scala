package baboon.runtime.shared {

  case class BaboonMethodId(serviceName: String, methodName: String)

  sealed trait BaboonWiringError
  object BaboonWiringError {
    case class NoMatchingMethod(method: BaboonMethodId) extends BaboonWiringError
    case class NoMatchingService(method: BaboonMethodId) extends BaboonWiringError
    case class DuplicateService(serviceName: String) extends BaboonWiringError
    case class DecoderFailed(method: BaboonMethodId, exception: Throwable) extends BaboonWiringError
    case class EncoderFailed(method: BaboonMethodId, exception: Throwable) extends BaboonWiringError
    case class CallFailed(method: BaboonMethodId, domainError: Any) extends BaboonWiringError
  }

  class BaboonWiringException(val error: BaboonWiringError) extends RuntimeException(error.toString)

  // --- Service muxers ---
  //
  // Cross-domain composable dispatch. A muxer holds a set of services from
  // any model(s) and routes an `(method, data, ctx)` call to the right one
  // by `method.serviceName`. The `R` type parameter encodes the per-call
  // return shape so the same class supports both sync (`R = String`,
  // `R = Array[Byte]`) and effectful (`R = F[BaboonWiringError, String]`,
  // …) generated services. The per-service wrapper classes emitted
  // alongside `${Svc}Wiring.invokeJson` / `invokeUeba` carry the matching
  // parameterisation.

  trait IBaboonJsonService[R] {
    def serviceName: String
    def invoke(method: BaboonMethodId, data: String, ctx: BaboonCodecContext): R
  }

  trait IBaboonUebaService[R] {
    def serviceName: String
    def invoke(method: BaboonMethodId, data: Array[Byte], ctx: BaboonCodecContext): R
  }

  final class JsonMuxer[R](services: IBaboonJsonService[R]*) {
    private val table = scala.collection.mutable.LinkedHashMap.empty[String, IBaboonJsonService[R]]
    services.foreach(register)

    def register(service: IBaboonJsonService[R]): Unit = {
      if (table.contains(service.serviceName)) {
        throw new BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
      }
      table.update(service.serviceName, service)
    }

    def invoke(method: BaboonMethodId, data: String, ctx: BaboonCodecContext): R = {
      table.get(method.serviceName) match {
        case Some(service) => service.invoke(method, data, ctx)
        case None          => throw new BaboonWiringException(BaboonWiringError.NoMatchingService(method))
      }
    }

    def serviceNames: Seq[String] = table.keys.toVector
  }

  final class UebaMuxer[R](services: IBaboonUebaService[R]*) {
    private val table = scala.collection.mutable.LinkedHashMap.empty[String, IBaboonUebaService[R]]
    services.foreach(register)

    def register(service: IBaboonUebaService[R]): Unit = {
      if (table.contains(service.serviceName)) {
        throw new BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
      }
      table.update(service.serviceName, service)
    }

    def invoke(method: BaboonMethodId, data: Array[Byte], ctx: BaboonCodecContext): R = {
      table.get(method.serviceName) match {
        case Some(service) => service.invoke(method, data, ctx)
        case None          => throw new BaboonWiringException(BaboonWiringError.NoMatchingService(method))
      }
    }

    def serviceNames: Seq[String] = table.keys.toVector
  }

  // --- Context-carrying service muxers ---
  //
  // Emitted when a service.context mode (`abstract` or `type`) is active. The
  // abstract/concrete service context `Ctx` is supplied per-invocation
  // (alongside the codec context) rather than baked into the wrapper, so
  // callers thread a fresh context through each dispatch. The context-free
  // interfaces above are left untouched so `--service-context-mode none`
  // output (and the service-acceptance matrix) is byte-identical.

  trait IBaboonJsonServiceCtx[Ctx, R] {
    def serviceName: String
    def invoke(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): R
  }

  trait IBaboonUebaServiceCtx[Ctx, R] {
    def serviceName: String
    def invoke(method: BaboonMethodId, data: Array[Byte], ctx: Ctx, codecCtx: BaboonCodecContext): R
  }

  final class JsonMuxerCtx[Ctx, R](services: IBaboonJsonServiceCtx[Ctx, R]*) {
    private val table = scala.collection.mutable.LinkedHashMap.empty[String, IBaboonJsonServiceCtx[Ctx, R]]
    services.foreach(register)

    def register(service: IBaboonJsonServiceCtx[Ctx, R]): Unit = {
      if (table.contains(service.serviceName)) {
        throw new BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
      }
      table.update(service.serviceName, service)
    }

    def invoke(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): R = {
      table.get(method.serviceName) match {
        case Some(service) => service.invoke(method, data, ctx, codecCtx)
        case None          => throw new BaboonWiringException(BaboonWiringError.NoMatchingService(method))
      }
    }

    def serviceNames: Seq[String] = table.keys.toVector
  }

  final class UebaMuxerCtx[Ctx, R](services: IBaboonUebaServiceCtx[Ctx, R]*) {
    private val table = scala.collection.mutable.LinkedHashMap.empty[String, IBaboonUebaServiceCtx[Ctx, R]]
    services.foreach(register)

    def register(service: IBaboonUebaServiceCtx[Ctx, R]): Unit = {
      if (table.contains(service.serviceName)) {
        throw new BaboonWiringException(BaboonWiringError.DuplicateService(service.serviceName))
      }
      table.update(service.serviceName, service)
    }

    def invoke(method: BaboonMethodId, data: Array[Byte], ctx: Ctx, codecCtx: BaboonCodecContext): R = {
      table.get(method.serviceName) match {
        case Some(service) => service.invoke(method, data, ctx, codecCtx)
        case None          => throw new BaboonWiringException(BaboonWiringError.NoMatchingService(method))
      }
    }

    def serviceNames: Seq[String] = table.keys.toVector
  }

}
