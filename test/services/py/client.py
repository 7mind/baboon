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


def run_client(host: str, port: int) -> None:
    # Reset
    post(host, port, "/reset", "")

    client = PetStoreClient(
        lambda service, method, data: post(host, port, f"/{service}/{method}", data),
        ctx,
    )

    # Add Buddy
    add_buddy_out = client.addPet_json(AddPetIn(name="Buddy", status=PetStatus.Available, tag="dog"))
    buddy_id = add_buddy_out.pet.id
    assert add_buddy_out.pet.name == "Buddy", f"expected name Buddy, got {add_buddy_out.pet.name}"

    # Add Whiskers
    add_whiskers_out = client.addPet_json(AddPetIn(name="Whiskers", status=PetStatus.Pending, tag="cat"))
    whiskers_id = add_whiskers_out.pet.id
    assert add_whiskers_out.pet.name == "Whiskers", f"expected name Whiskers, got {add_whiskers_out.pet.name}"

    # List pets (expect 2)
    list_out = client.listPets_json(ListPetsIn())
    assert len(list_out.pets) == 2, f"expected 2 pets, got {len(list_out.pets)}"

    # Get Buddy
    get_buddy_out = client.getPet_json(GetPetIn(id=buddy_id))
    assert get_buddy_out.pet.name == "Buddy", f"expected Buddy, got {get_buddy_out.pet.name}"
    assert get_buddy_out.pet.status == PetStatus.Available, f"expected Available, got {get_buddy_out.pet.status}"
    assert get_buddy_out.pet.tag == "dog", f"expected tag dog, got {get_buddy_out.pet.tag}"

    # Delete Whiskers
    delete_out = client.deletePet_json(DeletePetIn(id=whiskers_id))
    assert delete_out.deleted is True, f"expected deleted=True, got {delete_out.deleted}"

    # List pets again (expect 1)
    list2_out = client.listPets_json(ListPetsIn())
    assert len(list2_out.pets) == 1, f"expected 1 pet, got {len(list2_out.pets)}"
    assert list2_out.pets[0].name == "Buddy", f"expected remaining pet Buddy, got {list2_out.pets[0].name}"

    print("OK")
