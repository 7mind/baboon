import json
import urllib.request
from Generated.baboon_codecs import BaboonCodecContext
from Generated.petstore.api.PetStatus import PetStatus
from Generated.petstore.api.petstore.addpet.In import In as AddPetIn, In_JsonCodec as AddPetInCodec
from Generated.petstore.api.petstore.addpet.Out import Out_JsonCodec as AddPetOutCodec
from Generated.petstore.api.petstore.getpet.In import In as GetPetIn, In_JsonCodec as GetPetInCodec
from Generated.petstore.api.petstore.getpet.Out import Out_JsonCodec as GetPetOutCodec
from Generated.petstore.api.petstore.listpets.In import In as ListPetsIn, In_JsonCodec as ListPetsInCodec
from Generated.petstore.api.petstore.listpets.Out import Out_JsonCodec as ListPetsOutCodec
from Generated.petstore.api.petstore.deletepet.In import In as DeletePetIn, In_JsonCodec as DeletePetInCodec
from Generated.petstore.api.petstore.deletepet.Out import Out_JsonCodec as DeletePetOutCodec

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

    # Add Buddy
    add_buddy_in = AddPetIn(name="Buddy", status=PetStatus.Available, tag="dog")
    add_buddy_resp = post(host, port, "/PetStore/addPet",
                          AddPetInCodec.instance().encode(ctx, add_buddy_in))
    add_buddy_out = AddPetOutCodec.instance().decode(ctx, add_buddy_resp)
    buddy_id = add_buddy_out.pet.id
    assert add_buddy_out.pet.name == "Buddy", f"expected name Buddy, got {add_buddy_out.pet.name}"

    # Add Whiskers
    add_whiskers_in = AddPetIn(name="Whiskers", status=PetStatus.Pending, tag="cat")
    add_whiskers_resp = post(host, port, "/PetStore/addPet",
                             AddPetInCodec.instance().encode(ctx, add_whiskers_in))
    add_whiskers_out = AddPetOutCodec.instance().decode(ctx, add_whiskers_resp)
    whiskers_id = add_whiskers_out.pet.id
    assert add_whiskers_out.pet.name == "Whiskers", f"expected name Whiskers, got {add_whiskers_out.pet.name}"

    # List pets (expect 2)
    list_in = ListPetsIn()
    list_resp = post(host, port, "/PetStore/listPets",
                     ListPetsInCodec.instance().encode(ctx, list_in))
    list_out = ListPetsOutCodec.instance().decode(ctx, list_resp)
    assert len(list_out.pets) == 2, f"expected 2 pets, got {len(list_out.pets)}"

    # Get Buddy
    get_buddy_in = GetPetIn(id=buddy_id)
    get_buddy_resp = post(host, port, "/PetStore/getPet",
                          GetPetInCodec.instance().encode(ctx, get_buddy_in))
    get_buddy_out = GetPetOutCodec.instance().decode(ctx, get_buddy_resp)
    assert get_buddy_out.pet.name == "Buddy", f"expected Buddy, got {get_buddy_out.pet.name}"
    assert get_buddy_out.pet.status == PetStatus.Available, f"expected Available, got {get_buddy_out.pet.status}"
    assert get_buddy_out.pet.tag == "dog", f"expected tag dog, got {get_buddy_out.pet.tag}"

    # Delete Whiskers
    delete_in = DeletePetIn(id=whiskers_id)
    delete_resp = post(host, port, "/PetStore/deletePet",
                       DeletePetInCodec.instance().encode(ctx, delete_in))
    delete_out = DeletePetOutCodec.instance().decode(ctx, delete_resp)
    assert delete_out.deleted is True, f"expected deleted=True, got {delete_out.deleted}"

    # List pets again (expect 1)
    list2_resp = post(host, port, "/PetStore/listPets",
                      ListPetsInCodec.instance().encode(ctx, list_in))
    list2_out = ListPetsOutCodec.instance().decode(ctx, list2_resp)
    assert len(list2_out.pets) == 1, f"expected 1 pet, got {len(list2_out.pets)}"
    assert list2_out.pets[0].name == "Buddy", f"expected remaining pet Buddy, got {list2_out.pets[0].name}"

    print("OK")
