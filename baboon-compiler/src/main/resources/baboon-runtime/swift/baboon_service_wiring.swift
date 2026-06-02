import Foundation

// --- Service Wiring ---

public struct BaboonMethodId: Equatable, Hashable {
    public let serviceId: String
    public let methodName: String

    public init(serviceId: String, methodName: String) {
        self.serviceId = serviceId
        self.methodName = methodName
    }
}

public enum BaboonWiringError: Error {
    case noMatchingMethod(BaboonMethodId)
    case noMatchingService(BaboonMethodId)
    case duplicateService(String)
    case decoderFailed(BaboonMethodId, Error)
    case encoderFailed(BaboonMethodId, Error)
    case callFailed(BaboonMethodId, Any)
}

public struct BaboonWiringException: Error {
    public let error: BaboonWiringError
    public init(_ error: BaboonWiringError) { self.error = error }
}

// --- Service muxers ---
//
// Cross-domain composable dispatch. A muxer holds a set of services from any
// model(s) and routes a `(method, data, ctx)` call to the right one by
// `method.serviceId`. The `R` type parameter encodes the return shape so the
// same class supports both modes — pass `JsonMuxer<String>` for noErrors-mode
// throwing services, or `JsonMuxer<BaboonEither<BaboonWiringError, String>>`
// for errors-mode non-throwing services. The per-service wrapper classes
// emitted alongside `<Svc>Wiring.invokeJson` / `<Svc>Wiring.invokeUeba` carry
// the matching parameterisation.

public protocol IBaboonJsonService {
    associatedtype R
    var serviceName: String { get }
    func invoke(_ method: BaboonMethodId, _ data: String, _ ctx: BaboonCodecContext) throws -> R
}

public protocol IBaboonUebaService {
    associatedtype R
    var serviceName: String { get }
    func invoke(_ method: BaboonMethodId, _ data: Data, _ ctx: BaboonCodecContext) throws -> R
}

// Type-erasing boxes — Swift `protocol` with `associatedtype` cannot be used
// as an existential pre-Swift-5.7, and even on newer compilers a homogeneous
// `[any IBaboonJsonService]<R>` storage needs erasure to keep the muxer
// generic over a single `R`. The boxes capture the closure and the
// `serviceName` so the muxer's storage stays a flat `[String: AnyJsonService<R>]`.

public struct AnyJsonService<R> {
    public let serviceName: String
    private let _invoke: (BaboonMethodId, String, BaboonCodecContext) throws -> R

    public init<S: IBaboonJsonService>(_ s: S) where S.R == R {
        self.serviceName = s.serviceName
        self._invoke = s.invoke
    }

    public init(serviceName: String, invoke: @escaping (BaboonMethodId, String, BaboonCodecContext) throws -> R) {
        self.serviceName = serviceName
        self._invoke = invoke
    }

    public func invoke(_ method: BaboonMethodId, _ data: String, _ ctx: BaboonCodecContext) throws -> R {
        return try _invoke(method, data, ctx)
    }
}

public struct AnyUebaService<R> {
    public let serviceName: String
    private let _invoke: (BaboonMethodId, Data, BaboonCodecContext) throws -> R

    public init<S: IBaboonUebaService>(_ s: S) where S.R == R {
        self.serviceName = s.serviceName
        self._invoke = s.invoke
    }

    public init(serviceName: String, invoke: @escaping (BaboonMethodId, Data, BaboonCodecContext) throws -> R) {
        self.serviceName = serviceName
        self._invoke = invoke
    }

    public func invoke(_ method: BaboonMethodId, _ data: Data, _ ctx: BaboonCodecContext) throws -> R {
        return try _invoke(method, data, ctx)
    }
}

public class JsonMuxer<R> {
    private var table: [String: AnyJsonService<R>] = [:]

    public init(_ services: AnyJsonService<R>...) throws {
        for s in services { try register(s) }
    }

    public init(_ services: [AnyJsonService<R>]) throws {
        for s in services { try register(s) }
    }

    public func register(_ service: AnyJsonService<R>) throws {
        if table[service.serviceName] != nil {
            throw BaboonWiringException(BaboonWiringError.duplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    public func invoke(_ method: BaboonMethodId, _ data: String, _ ctx: BaboonCodecContext) throws -> R {
        guard let svc = table[method.serviceId] else {
            throw BaboonWiringException(BaboonWiringError.noMatchingService(method))
        }
        return try svc.invoke(method, data, ctx)
    }

    public func serviceNames() -> [String] {
        return Array(table.keys)
    }
}

public class UebaMuxer<R> {
    private var table: [String: AnyUebaService<R>] = [:]

    public init(_ services: AnyUebaService<R>...) throws {
        for s in services { try register(s) }
    }

    public init(_ services: [AnyUebaService<R>]) throws {
        for s in services { try register(s) }
    }

    public func register(_ service: AnyUebaService<R>) throws {
        if table[service.serviceName] != nil {
            throw BaboonWiringException(BaboonWiringError.duplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    public func invoke(_ method: BaboonMethodId, _ data: Data, _ ctx: BaboonCodecContext) throws -> R {
        guard let svc = table[method.serviceId] else {
            throw BaboonWiringException(BaboonWiringError.noMatchingService(method))
        }
        return try svc.invoke(method, data, ctx)
    }

    public func serviceNames() -> [String] {
        return Array(table.keys)
    }
}

// Context-carrying service variants, emitted when a service.context mode
// (`abstract` or `type`) is active. The service context `Ctx` is supplied
// per-invocation (alongside the codec context) rather than baked into the
// wrapper at construction, so callers thread a fresh context through each
// dispatch. The context-free protocols/muxers above are left untouched so
// `--service-context-mode none` output (and the service-acceptance matrix)
// stays byte-identical.
//
// Both `Ctx` and `R` are associated types on the protocol; the per-service
// wrapper class (emitted alongside `<Svc>Wiring.invoke{Json,Ueba}`) is generic
// over the concrete `Impl: <Svc>` in abstract mode and binds `Ctx = Impl.Ctx`,
// or non-generic with `Ctx = <ConcreteType>` in `type` mode. The type-erasing
// `Any*ServiceCtx<Ctx, R>` boxes keep the muxer storage flat over a single
// `(Ctx, R)` pair, mirroring the context-free `Any{Json,Ueba}Service<R>`.

public protocol IBaboonJsonServiceCtx {
    associatedtype Ctx
    associatedtype R
    var serviceName: String { get }
    func invoke(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R
}

public protocol IBaboonUebaServiceCtx {
    associatedtype Ctx
    associatedtype R
    var serviceName: String { get }
    func invoke(_ method: BaboonMethodId, _ data: Data, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R
}

public struct AnyJsonServiceCtx<Ctx, R> {
    public let serviceName: String
    private let _invoke: (BaboonMethodId, String, Ctx, BaboonCodecContext) throws -> R

    public init<S: IBaboonJsonServiceCtx>(_ s: S) where S.Ctx == Ctx, S.R == R {
        self.serviceName = s.serviceName
        self._invoke = s.invoke
    }

    public init(serviceName: String, invoke: @escaping (BaboonMethodId, String, Ctx, BaboonCodecContext) throws -> R) {
        self.serviceName = serviceName
        self._invoke = invoke
    }

    public func invoke(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R {
        return try _invoke(method, data, ctx, codecCtx)
    }
}

public struct AnyUebaServiceCtx<Ctx, R> {
    public let serviceName: String
    private let _invoke: (BaboonMethodId, Data, Ctx, BaboonCodecContext) throws -> R

    public init<S: IBaboonUebaServiceCtx>(_ s: S) where S.Ctx == Ctx, S.R == R {
        self.serviceName = s.serviceName
        self._invoke = s.invoke
    }

    public init(serviceName: String, invoke: @escaping (BaboonMethodId, Data, Ctx, BaboonCodecContext) throws -> R) {
        self.serviceName = serviceName
        self._invoke = invoke
    }

    public func invoke(_ method: BaboonMethodId, _ data: Data, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R {
        return try _invoke(method, data, ctx, codecCtx)
    }
}

public class JsonMuxerCtx<Ctx, R> {
    private var table: [String: AnyJsonServiceCtx<Ctx, R>] = [:]

    public init(_ services: AnyJsonServiceCtx<Ctx, R>...) throws {
        for s in services { try register(s) }
    }

    public init(_ services: [AnyJsonServiceCtx<Ctx, R>]) throws {
        for s in services { try register(s) }
    }

    public func register(_ service: AnyJsonServiceCtx<Ctx, R>) throws {
        if table[service.serviceName] != nil {
            throw BaboonWiringException(BaboonWiringError.duplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    public func invoke(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R {
        guard let svc = table[method.serviceId] else {
            throw BaboonWiringException(BaboonWiringError.noMatchingService(method))
        }
        return try svc.invoke(method, data, ctx, codecCtx)
    }

    public func serviceNames() -> [String] {
        return Array(table.keys)
    }
}

public class UebaMuxerCtx<Ctx, R> {
    private var table: [String: AnyUebaServiceCtx<Ctx, R>] = [:]

    public init(_ services: AnyUebaServiceCtx<Ctx, R>...) throws {
        for s in services { try register(s) }
    }

    public init(_ services: [AnyUebaServiceCtx<Ctx, R>]) throws {
        for s in services { try register(s) }
    }

    public func register(_ service: AnyUebaServiceCtx<Ctx, R>) throws {
        if table[service.serviceName] != nil {
            throw BaboonWiringException(BaboonWiringError.duplicateService(service.serviceName))
        }
        table[service.serviceName] = service
    }

    public func invoke(_ method: BaboonMethodId, _ data: Data, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> R {
        guard let svc = table[method.serviceId] else {
            throw BaboonWiringException(BaboonWiringError.noMatchingService(method))
        }
        return try svc.invoke(method, data, ctx, codecCtx)
    }

    public func serviceNames() -> [String] {
        return Array(table.keys)
    }
}

