"""In-process round-trip test for the generated PetStore RPC client.

Exercises the generated `PetStoreClient` (UEBA + JSON method variants) against an
in-process transport that delegates to the generated server-side wiring invokers
(`invoke_ueba_PetStore` / `invoke_json_PetStore`). No sockets: the transport
callback feeds the client-encoded bytes/str straight into the wiring and returns
the wiring's encoded response for the client to decode.

Run directly: `python3 client_roundtrip.py`
"""
from Generated.baboon_codecs import BaboonCodecContext
from Generated.baboon_service_wiring import BaboonMethodId
from Generated.petstore.api.PetStore import PetStore
from Generated.petstore.api.PetStore_Client import PetStoreClient
from Generated.petstore.api import PetStore_Wiring

invoke_json_PetStore = getattr(PetStore_Wiring, "invoke_json_PetStore", None)
invoke_ueba_PetStore = getattr(PetStore_Wiring, "invoke_ueba_PetStore", None)

from Generated.petstore.api.Pet import Pet
from Generated.petstore.api.PetStatus import PetStatus
from Generated.petstore.api.petstore.addpet.In import In as AddPetIn
from Generated.petstore.api.petstore.addpet.Out import Out as AddPetOut
from Generated.petstore.api.petstore.getpet.In import In as GetPetIn
from Generated.petstore.api.petstore.getpet.Out import Out as GetPetOut
from Generated.petstore.api.petstore.listpets.In import In as ListPetsIn
from Generated.petstore.api.petstore.listpets.Out import Out as ListPetsOut
from Generated.petstore.api.petstore.deletepet.In import In as DeletePetIn
from Generated.petstore.api.petstore.deletepet.Out import Out as DeletePetOut


class PetStoreImpl(PetStore):
    def __init__(self) -> None:
        self._pets: dict[int, Pet] = {}
        self._next_id: int = 1

    def addPet(self, arg: AddPetIn) -> AddPetOut:
        pet_id = self._next_id
        self._next_id += 1
        pet = Pet(id=pet_id, name=arg.name, status=arg.status, tag=arg.tag)
        self._pets[pet_id] = pet
        return AddPetOut(pet=pet)

    def getPet(self, arg: GetPetIn) -> GetPetOut:
        pet = self._pets.get(arg.id)
        if pet is None:
            raise KeyError(f"Pet not found: id={arg.id}")
        return GetPetOut(pet=pet)

    def listPets(self, arg: ListPetsIn) -> ListPetsOut:
        return ListPetsOut(pets=sorted(self._pets.values(), key=lambda p: p.id))

    def deletePet(self, arg: DeletePetIn) -> DeletePetOut:
        existed = arg.id in self._pets
        if existed:
            del self._pets[arg.id]
        return DeletePetOut(deleted=existed)


def _make_client(impl: PetStoreImpl, ctx: BaboonCodecContext) -> PetStoreClient:
    # The generated client constructor takes only the transports for the codecs
    # that are active (UEBA first, then JSON, then ctx). Build the positional
    # argument list to match whatever was generated.
    args: list = []
    if invoke_ueba_PetStore is not None:
        def transport_ueba(service: str, method: str, data: bytes) -> bytes:
            assert service == "PetStore", f"unexpected service {service}"
            return invoke_ueba_PetStore(BaboonMethodId(service, method), data, impl, ctx)
        args.append(transport_ueba)
    if invoke_json_PetStore is not None:
        def transport_json(service: str, method: str, data: str) -> str:
            assert service == "PetStore", f"unexpected service {service}"
            return invoke_json_PetStore(BaboonMethodId(service, method), data, impl, ctx)
        args.append(transport_json)
    args.append(ctx)
    return PetStoreClient(*args)


def _run(use_json: bool) -> None:
    impl = PetStoreImpl()
    ctx = BaboonCodecContext.compact()
    client = _make_client(impl, ctx)

    if use_json:
        add = client.addPet_json
        get = client.getPet_json
        lst = client.listPets_json
        dele = client.deletePet_json
        label = "JSON"
    else:
        add = client.addPet
        get = client.getPet
        lst = client.listPets
        dele = client.deletePet
        label = "UEBA"

    buddy = add(AddPetIn(name="Buddy", status=PetStatus.Available, tag="dog"))
    assert buddy.pet.name == "Buddy", f"[{label}] expected Buddy, got {buddy.pet.name}"
    buddy_id = buddy.pet.id

    whiskers = add(AddPetIn(name="Whiskers", status=PetStatus.Pending, tag="cat"))
    whiskers_id = whiskers.pet.id

    listed = lst(ListPetsIn())
    assert len(listed.pets) == 2, f"[{label}] expected 2 pets, got {len(listed.pets)}"

    got = get(GetPetIn(id=buddy_id))
    assert got.pet.name == "Buddy", f"[{label}] expected Buddy, got {got.pet.name}"
    assert got.pet.status == PetStatus.Available, f"[{label}] expected Available, got {got.pet.status}"
    assert got.pet.tag == "dog", f"[{label}] expected tag dog, got {got.pet.tag}"

    deleted = dele(DeletePetIn(id=whiskers_id))
    assert deleted.deleted is True, f"[{label}] expected deleted=True, got {deleted.deleted}"

    listed2 = lst(ListPetsIn())
    assert len(listed2.pets) == 1, f"[{label}] expected 1 pet, got {len(listed2.pets)}"
    assert listed2.pets[0].name == "Buddy", f"[{label}] expected remaining Buddy, got {listed2.pets[0].name}"

    print(f"{label} client round-trip OK")


def run_client_roundtrip() -> None:
    ran = False
    if invoke_ueba_PetStore is not None:
        _run(use_json=False)
        ran = True
    if invoke_json_PetStore is not None:
        _run(use_json=True)
        ran = True
    assert ran, "no active codecs: neither JSON nor UEBA wiring was generated"
    print("OK")


if __name__ == "__main__":
    run_client_roundtrip()
