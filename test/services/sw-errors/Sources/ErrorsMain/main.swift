import Foundation
import BaboonRuntime
import Generated

// Concrete IBaboonServiceRt over the generated BaboonEither result container.
// The generated errors-mode wiring receives this `rt` and threads call results
// through pure/fail/leftMap/flatMap.
final class EitherRt: IBaboonServiceRt {
    func pure<L, R>(_ value: R) -> BaboonEither<L, R> { .right(value) }
    func fail<L, R>(_ error: L) -> BaboonEither<L, R> { .left(error) }
    func leftMap<A, B, C>(_ value: BaboonEither<A, B>, _ f: (A) -> C) -> BaboonEither<C, B> {
        switch value {
        case .left(let l):  return .left(f(l))
        case .right(let r): return .right(r)
        }
    }
    func flatMap<A, B, C>(_ value: BaboonEither<A, B>, _ f: (B) -> BaboonEither<A, C>) -> BaboonEither<A, C> {
        switch value {
        case .left(let l):  return .left(l)
        case .right(let r): return f(r)
        }
    }
}

// Errors-mode impl conforming to the container-returning PetStore protocol:
// each method returns BaboonEither<Err, Out> (DEFECT 2 fix). The wiring's
// rt.leftMap consumes that container soundly.
final class PetStoreErrorsImpl: PetStore {
    private var pets: [Int64: Pet] = [:]
    private var nextId: Int64 = 1

    func addPet(arg: petstore.addpet.`in`) -> BaboonEither<petstore.addpet.err, petstore.addpet.out> {
        let id = nextId
        nextId += 1
        let pet = Pet(id: id, name: arg.name, status: arg.status, tag: arg.tag)
        pets[id] = pet
        return .right(petstore.addpet.out(pet: pet))
    }

    func getPet(arg: petstore.getpet.`in`) -> BaboonEither<petstore.getpet.err, petstore.getpet.out> {
        guard let pet = pets[arg.id] else {
            return .left(petstore.getpet.err(problem: PetError(code: 404, message: "not found")))
        }
        return .right(petstore.getpet.out(pet: pet))
    }
}

func runErrorsWiringSmoke() throws {
    let impl = PetStoreErrorsImpl()
    let rt = EitherRt()
    let ctx = BaboonCodecContext.defaultCtx

    // 1) Errors-mode invoke dispatcher (JSON): returns BaboonEither<BaboonWiringError, String>.
    let addM = BaboonMethodId(serviceId: "PetStore", methodName: "addPet")
    let addJson = "{\"name\":\"Buddy\",\"status\":\"Available\",\"tag\":\"dog\"}"
    let addOut: BaboonEither<BaboonWiringError, String> =
        PetStoreWiring.invokeJson(addM, addJson, impl, rt, ctx)
    switch addOut {
    case .right(let s): assert(s.contains("Buddy"), "dispatcher json missing pet name: \(s)")
    case .left(let e):  fatalError("dispatcher failed: \(e)")
    }

    // 2) Muxer-wrapper: invoke() returns R = BaboonEither<BaboonWiringError, String>.
    let wrapper = PetStoreJsonService(impl, rt: rt)
    let wrapped: BaboonEither<BaboonWiringError, String> =
        try wrapper.invoke(addM, "{\"name\":\"Whiskers\",\"status\":\"Pending\",\"tag\":\"cat\"}", ctx)
    if case .left(let e) = wrapped { fatalError("wrapper failed: \(e)") }

    // 3) UEBA dispatcher path (forces the errors-mode UEBA wiring to type-check too).
    let writer = BaboonBinTools.createWriter()
    petstore.addpet.in_UebaCodec.instance.encode(ctx, writer, petstore.addpet.`in`(name: "Rex", status: .Sold, tag: nil))
    let addUeba: BaboonEither<BaboonWiringError, Data> =
        PetStoreWiring.invokeUeba(addM, writer.toData(), impl, rt, ctx)
    if case .left(let e) = addUeba { fatalError("ueba dispatcher failed: \(e)") }

    print("OK")
}

try runErrorsWiringSmoke()
