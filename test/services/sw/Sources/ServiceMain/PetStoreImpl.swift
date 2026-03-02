import Foundation
import BaboonRuntime
import Generated

final class PetStoreImpl: PetStore {
    private var pets: [Int64: Pet] = [:]
    private var nextId: Int64 = 1

    func reset() {
        pets.removeAll()
        nextId = 1
    }

    func addPet(arg: petstore.addpet.`in`) -> petstore.addpet.out {
        let id = nextId
        nextId += 1
        let pet = Pet(id: id, name: arg.name, status: arg.status, tag: arg.tag)
        pets[id] = pet
        return petstore.addpet.out(pet: pet)
    }

    func getPet(arg: petstore.getpet.`in`) -> petstore.getpet.out {
        guard let pet = pets[arg.id] else {
            fatalError("Pet not found: id=\(arg.id)")
        }
        return petstore.getpet.out(pet: pet)
    }

    func listPets(arg: petstore.listpets.`in`) -> petstore.listpets.out {
        let sorted = pets.values.sorted { $0.id < $1.id }
        return petstore.listpets.out(pets: sorted)
    }

    func deletePet(arg: petstore.deletepet.`in`) -> petstore.deletepet.out {
        let existed = pets.removeValue(forKey: arg.id) != nil
        return petstore.deletepet.out(deleted: existed)
    }
}
