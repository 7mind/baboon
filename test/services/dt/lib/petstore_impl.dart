import 'generated/petstore/api/pet.dart';
import 'generated/petstore/api/petstore/addpet/in.dart' as addpet;
import 'generated/petstore/api/petstore/addpet/out.dart' as addpet;
import 'generated/petstore/api/petstore/getpet/in.dart' as getpet;
import 'generated/petstore/api/petstore/getpet/out.dart' as getpet;
import 'generated/petstore/api/petstore/listpets/in.dart' as listpets;
import 'generated/petstore/api/petstore/listpets/out.dart' as listpets;
import 'generated/petstore/api/petstore/deletepet/in.dart' as deletepet;
import 'generated/petstore/api/petstore/deletepet/out.dart' as deletepet;

class PetStoreImpl {
  final Map<int, Pet> _pets = {};
  int _nextId = 1;

  void reset() {
    _pets.clear();
    _nextId = 1;
  }

  addpet.out addPet(addpet.in_ arg) {
    final petId = _nextId;
    _nextId += 1;
    final pet = Pet(id: petId, name: arg.name, status: arg.status, tag: arg.tag);
    _pets[petId] = pet;
    return addpet.out(pet: pet);
  }

  getpet.out getPet(getpet.in_ arg) {
    final pet = _pets[arg.id];
    if (pet == null) {
      throw StateError('Pet not found: id=${arg.id}');
    }
    return getpet.out(pet: pet);
  }

  listpets.out listPets(listpets.in_ arg) {
    final sorted = _pets.values.toList()..sort((a, b) => a.id.compareTo(b.id));
    return listpets.out(pets: sorted);
  }

  deletepet.out deletePet(deletepet.in_ arg) {
    final existed = _pets.containsKey(arg.id);
    if (existed) {
      _pets.remove(arg.id);
    }
    return deletepet.out(deleted: existed);
  }
}
