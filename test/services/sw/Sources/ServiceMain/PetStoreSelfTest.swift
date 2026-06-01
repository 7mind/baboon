import Foundation
import BaboonRuntime
import Generated

// In-process round-trip test for the generated `PetStoreClient`.
//
// Wires the generated client's JSON transport callback directly to the
// generated `PetStoreWiring.invokeJson` server entry point over a shared
// in-memory `PetStoreImpl` — no sockets, no HTTP. This exercises the full
// client path: encode input -> transport -> server decode/dispatch/encode ->
// client decode output.
//
// The service-acceptance harness generates Swift with JSON codecs only
// (`--generate-json-codecs-by-default=true`, no UEBA-by-default), so this
// self-test drives the JSON flavour, which is the codec flavour emitted under
// those flags. The UEBA client methods (bare endpoint name) are generated and
// compile identically when UEBA codecs are active; the JSON path here proves
// the generated client wiring end to end.
func runSelfTest() {
    let ctx = BaboonCodecContext.defaultCtx
    let impl = PetStoreImpl()

    // In-process transports: route (service, method, payload) to the generated
    // server wiring against the shared impl. Both codecs are active, so the
    // generated client requires both transports.
    let client = PetStoreClient(
        transportUeba: { service, method, data in
            try PetStoreWiring.invokeUeba(
                BaboonMethodId(serviceId: service, methodName: method),
                data,
                impl,
                ctx
            )
        },
        transportJson: { service, method, data in
            try PetStoreWiring.invokeJson(
                BaboonMethodId(serviceId: service, methodName: method),
                data,
                impl,
                ctx
            )
        }
    )

    impl.reset()

    // addPet (Buddy)
    let buddyOut = try! client.addPetJson(
        arg: petstore.addpet.`in`(name: "Buddy", status: .Available, tag: "dog")
    )
    precondition(buddyOut.pet.name == "Buddy", "expected name Buddy, got \(buddyOut.pet.name)")
    let buddyId = buddyOut.pet.id

    // addPet (Whiskers)
    let whiskersOut = try! client.addPetJson(
        arg: petstore.addpet.`in`(name: "Whiskers", status: .Pending, tag: "cat")
    )
    precondition(whiskersOut.pet.name == "Whiskers", "expected name Whiskers, got \(whiskersOut.pet.name)")
    let whiskersId = whiskersOut.pet.id

    // listPets -> 2
    let list = try! client.listPetsJson(arg: petstore.listpets.`in`())
    precondition(list.pets.count == 2, "expected 2 pets, got \(list.pets.count)")

    // getPet (Buddy)
    let getBuddy = try! client.getPetJson(arg: petstore.getpet.`in`(id: buddyId))
    precondition(getBuddy.pet.name == "Buddy", "expected Buddy, got \(getBuddy.pet.name)")
    precondition(getBuddy.pet.status == .Available, "expected Available, got \(getBuddy.pet.status)")
    precondition(getBuddy.pet.tag == "dog", "expected tag dog, got \(String(describing: getBuddy.pet.tag))")

    // deletePet (Whiskers)
    let del = try! client.deletePetJson(arg: petstore.deletepet.`in`(id: whiskersId))
    precondition(del.deleted == true, "expected deleted=true, got \(del.deleted)")

    // listPets -> 1
    let list2 = try! client.listPetsJson(arg: petstore.listpets.`in`())
    precondition(list2.pets.count == 1, "expected 1 pet, got \(list2.pets.count)")
    precondition(list2.pets[0].name == "Buddy", "expected remaining pet Buddy, got \(list2.pets[0].name)")

    print("OK")
}
