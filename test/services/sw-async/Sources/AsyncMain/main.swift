import Foundation
import BaboonRuntime
import Generated

// Async impl conforming to the now-`async throws` PetStore protocol (emitted
// when --sw-async-services=true).
final class PetStoreAsyncImpl: PetStore {
    private var pets: [Int64: Pet] = [:]
    private var nextId: Int64 = 1

    func addPet(arg: petstore.addpet.`in`) async throws -> petstore.addpet.out {
        let id = nextId
        nextId += 1
        let pet = Pet(id: id, name: arg.name, status: arg.status, tag: arg.tag)
        pets[id] = pet
        return petstore.addpet.out(pet: pet)
    }

    func getPet(arg: petstore.getpet.`in`) async throws -> petstore.getpet.out {
        guard let pet = pets[arg.id] else { fatalError("Pet not found: id=\(arg.id)") }
        return petstore.getpet.out(pet: pet)
    }

    func listPets(arg: petstore.listpets.`in`) async throws -> petstore.listpets.out {
        return petstore.listpets.out(pets: pets.values.sorted { $0.id < $1.id })
    }

    func deletePet(arg: petstore.deletepet.`in`) async throws -> petstore.deletepet.out {
        return petstore.deletepet.out(deleted: pets.removeValue(forKey: arg.id) != nil)
    }
}

func runAsyncWiringSmoke() async throws {
    let impl = PetStoreAsyncImpl()
    let ctx = BaboonCodecContext.defaultCtx

    // 1) Async invoke dispatcher (JSON).
    let addM = BaboonMethodId(serviceId: "PetStore", methodName: "addPet")
    let addJson = "{\"name\":\"Buddy\",\"status\":\"Available\",\"tag\":\"dog\"}"
    let addOut = try await PetStoreWiring.invokeJson(addM, addJson, impl, ctx)
    assert(addOut.contains("Buddy"), "dispatcher json missing pet name: \(addOut)")

    // 2) Muxer-wrapper thunk: invoke() returns `() async throws -> String`.
    let wrapper = PetStoreJsonService(impl)
    let thunk = try wrapper.invoke(addM, "{\"name\":\"Whiskers\",\"status\":\"Pending\",\"tag\":\"cat\"}", ctx)
    let wrapped = try await thunk()
    assert(wrapped.contains("Whiskers"), "wrapper thunk missing pet name: \(wrapped)")

    // 3) Async client over an in-process transport routed to the async dispatchers.
    let client = PetStoreClient(
        transportUeba: { svc, method, data in
            try await PetStoreWiring.invokeUeba(BaboonMethodId(serviceId: svc, methodName: method), data, impl, ctx)
        },
        transportJson: { svc, method, data in
            try await PetStoreWiring.invokeJson(BaboonMethodId(serviceId: svc, methodName: method), data, impl, ctx)
        }
    )
    let listJson = try await client.listPetsJson(arg: petstore.listpets.`in`())
    assert(listJson.pets.count == 3, "expected 3 pets, got \(listJson.pets.count)")
    let listUeba = try await client.listPets(arg: petstore.listpets.`in`())
    assert(listUeba.pets.count == 3, "expected 3 pets (ueba), got \(listUeba.pets.count)")

    print("OK")
}

try await runAsyncWiringSmoke()
