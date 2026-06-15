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

/// Async server implementation attempting to implement the generated PetStore
/// interface with Future<T> return types.
///
/// This FAILS to analyze because --dt-async-services=true does NOT yet make
/// the generated interface methods async (DtDefnTranslator emits
/// `addpet_out.out addPet(addpet_in.in_ arg);` unconditionally, and
/// DtServiceWiringTranslator.invokeJson is a sync `static String` dispatcher).
/// An async server impl returning Future<T> cannot satisfy the generated
/// synchronous interface — reproducing D25.
///
/// Expected dart-analyze error:
///   'addPet' ('Future<out> Function(in_)') isn't a valid override of
///   'PetStore.addPet' ('out Function(in_)').
class PetStoreAsyncImpl implements PetStore {
  final Map<int, Pet> _pets = {};
  int _nextId = 1;

  void reset() {
    _pets.clear();
    _nextId = 1;
  }

  // These methods return Future<T>, but the generated PetStore interface
  // declares bare T return types. The Dart analyzer rejects overriding a
  // non-Future method with a Future-returning one:
  //   'addPet' ('Future<out> Function(in_)') isn't a valid override of
  //   'PetStore.addPet' ('out Function(in_)')
  // reproducing D25.

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
