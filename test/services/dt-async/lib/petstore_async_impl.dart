import 'generated/petstore/api/pet.dart';
import 'generated/petstore/api/pet_store.dart';
import 'generated/petstore/api/petstore/addpet/in.dart' as addpet;
import 'generated/petstore/api/petstore/addpet/out.dart' as addpet_out;
import 'generated/petstore/api/petstore/getpet/in.dart' as getpet;
import 'generated/petstore/api/petstore/getpet/out.dart' as getpet_out;
import 'generated/petstore/api/petstore/listpets/in.dart' as listpets;
import 'generated/petstore/api/petstore/listpets/out.dart' as listpets_out;
import 'generated/petstore/api/petstore/deletepet/in.dart' as deletepet;
import 'generated/petstore/api/petstore/deletepet/out.dart' as deletepet_out;

/// Async server implementation of the generated PetStore interface with
/// Future<T> return types.
///
/// With --dt-async-services=true (fixed in T80/T81), the generated PetStore
/// abstract class declares Future<T> return types on all methods and the
/// server dispatchers use async/await. This impl passes dart analyze and
/// serves as a GREEN regression guard for D25.
class PetStoreAsyncImpl implements PetStore {
  final Map<int, Pet> _pets = {};
  int _nextId = 1;

  void reset() {
    _pets.clear();
    _nextId = 1;
  }

  @override
  Future<addpet_out.out> addPet(addpet.in_ arg) async {
    final petId = _nextId;
    _nextId += 1;
    final pet = Pet(id: petId, name: arg.name, status: arg.status, tag: arg.tag);
    _pets[petId] = pet;
    return addpet_out.out(pet: pet);
  }

  @override
  Future<getpet_out.out> getPet(getpet.in_ arg) async {
    final pet = _pets[arg.id];
    if (pet == null) {
      throw StateError('Pet not found: id=${arg.id}');
    }
    return getpet_out.out(pet: pet);
  }

  @override
  Future<listpets_out.out> listPets(listpets.in_ arg) async {
    final sorted = _pets.values.toList()..sort((a, b) => a.id.compareTo(b.id));
    return listpets_out.out(pets: sorted);
  }

  @override
  Future<deletepet_out.out> deletePet(deletepet.in_ arg) async {
    final existed = _pets.containsKey(arg.id);
    if (existed) {
      _pets.remove(arg.id);
    }
    return deletepet_out.out(deleted: existed);
  }
}
