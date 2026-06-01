import urllib.request
from Generated.baboon_codecs import BaboonCodecContext
from Generated.petstore.api.PetStore_Client import PetStoreClient
from Generated.petstore.api.PetStatus import PetStatus
from Generated.petstore.api.petstore.addpet.In import In as AddPetIn
from Generated.petstore.api.petstore.getpet.In import In as GetPetIn
from Generated.petstore.api.petstore.listpets.In import In as ListPetsIn
from Generated.petstore.api.petstore.deletepet.In import In as DeletePetIn

ctx = BaboonCodecContext.compact()


def post(host: str, port: int, path: str, body: str) -> str:
    url = f"http://{host}:{port}{path}"
    data = body.encode("utf-8")
    req = urllib.request.Request(url, data=data, method="POST",
                                 headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req) as resp:
        return resp.read().decode("utf-8")


def post_bytes(host: str, port: int, path: str, body: bytes) -> bytes:
    url = f"http://{host}:{port}{path}"
    req = urllib.request.Request(url, data=body, method="POST",
                                 headers={"Content-Type": "application/octet-stream"})
    with urllib.request.urlopen(req) as resp:
        return resp.read()


def _scenario(label: str, add, get, lst, dele) -> None:
    # Add Buddy
    add_buddy_out = add(AddPetIn(name="Buddy", status=PetStatus.Available, tag="dog"))
    buddy_id = add_buddy_out.pet.id
    assert add_buddy_out.pet.name == "Buddy", f"[{label}] expected name Buddy, got {add_buddy_out.pet.name}"

    # Add Whiskers
    add_whiskers_out = add(AddPetIn(name="Whiskers", status=PetStatus.Pending, tag="cat"))
    whiskers_id = add_whiskers_out.pet.id
    assert add_whiskers_out.pet.name == "Whiskers", f"[{label}] expected name Whiskers, got {add_whiskers_out.pet.name}"

    # List pets (expect 2)
    list_out = lst(ListPetsIn())
    assert len(list_out.pets) == 2, f"[{label}] expected 2 pets, got {len(list_out.pets)}"

    # Get Buddy
    get_buddy_out = get(GetPetIn(id=buddy_id))
    assert get_buddy_out.pet.name == "Buddy", f"[{label}] expected Buddy, got {get_buddy_out.pet.name}"
    assert get_buddy_out.pet.status == PetStatus.Available, f"[{label}] expected Available, got {get_buddy_out.pet.status}"
    assert get_buddy_out.pet.tag == "dog", f"[{label}] expected tag dog, got {get_buddy_out.pet.tag}"

    # Delete Whiskers
    delete_out = dele(DeletePetIn(id=whiskers_id))
    assert delete_out.deleted is True, f"[{label}] expected deleted=True, got {delete_out.deleted}"

    # List pets again (expect 1)
    list2_out = lst(ListPetsIn())
    assert len(list2_out.pets) == 1, f"[{label}] expected 1 pet, got {len(list2_out.pets)}"
    assert list2_out.pets[0].name == "Buddy", f"[{label}] expected remaining pet Buddy, got {list2_out.pets[0].name}"


def run_client(host: str, port: int, codec: str = "both") -> None:
    transport_ueba = lambda service, method, data: post_bytes(host, port, f"/{service}/{method}", data)
    transport_json = lambda service, method, data: post(host, port, f"/{service}/{method}", data)
    client = PetStoreClient(transport_ueba, transport_json, ctx)

    run_json = codec in ("json", "both")
    run_ueba = codec in ("ueba", "both")

    if run_json:
        # JSON pass
        post(host, port, "/reset", "")
        _scenario("JSON", client.addPet_json, client.getPet_json, client.listPets_json, client.deletePet_json)

    if run_ueba:
        # UEBA pass — same scenario over the binary transport
        post(host, port, "/reset", "")
        _scenario("UEBA", client.addPet, client.getPet, client.listPets, client.deletePet)

    print("OK")
