package example

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.LEDataInputStream
import baboon.runtime.shared.LEDataOutputStream
import petstore.api.PetStatus
import petstore.api.PetStoreWiring
import petstore.api.BaboonMethodId
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

/**
 * In-process async round-trip driver for the Kotlin async wiring lane.
 * Exercises the generated PetStoreWiring async (suspend) dispatchers
 * (invokeJson + invokeUeba) end-to-end with the async PetStoreAsyncImpl.
 * All assertions throw explicitly to avoid vacuous-assert problems.
 */
fun main() = runBlocking {
    val ctx = BaboonCodecContext.Default
    val impl = PetStoreAsyncImpl()
    runUeba(impl, ctx)
    impl.reset()
    runJson(impl, ctx)
    println("OK")
}

private suspend fun runUeba(impl: PetStoreAsyncImpl, ctx: BaboonCodecContext) {
    // AddPet via UEBA
    val addIn = petstore.api.petstore.addpet.In(name = "Buddy", status = PetStatus.Available, tag = "dog")
    val addBaos = ByteArrayOutputStream()
    petstore.api.petstore.addpet.In_UEBACodec.encode(ctx, LEDataOutputStream(addBaos), addIn)
    val addBytes = PetStoreWiring.invokeUeba(BaboonMethodId("PetStore", "addPet"), addBaos.toByteArray(), impl, ctx)
    val addOut = petstore.api.petstore.addpet.Out_UEBACodec.decode(ctx, LEDataInputStream(ByteArrayInputStream(addBytes)))
    if (addOut.pet.name != "Buddy") throw AssertionError("UEBA AddPet name mismatch: ${addOut.pet.name}")
    val id = addOut.pet.id

    // GetPet via UEBA
    val getIn = petstore.api.petstore.getpet.In(id = id)
    val getBaos = ByteArrayOutputStream()
    petstore.api.petstore.getpet.In_UEBACodec.encode(ctx, LEDataOutputStream(getBaos), getIn)
    val getBytes = PetStoreWiring.invokeUeba(BaboonMethodId("PetStore", "getPet"), getBaos.toByteArray(), impl, ctx)
    val getOut = petstore.api.petstore.getpet.Out_UEBACodec.decode(ctx, LEDataInputStream(ByteArrayInputStream(getBytes)))
    if (getOut.pet.status != PetStatus.Available) throw AssertionError("UEBA GetPet status mismatch: ${getOut.pet.status}")

    // ListPets via UEBA
    val listIn = petstore.api.petstore.listpets.In()
    val listBaos = ByteArrayOutputStream()
    petstore.api.petstore.listpets.In_UEBACodec.encode(ctx, LEDataOutputStream(listBaos), listIn)
    val listBytes = PetStoreWiring.invokeUeba(BaboonMethodId("PetStore", "listPets"), listBaos.toByteArray(), impl, ctx)
    val listOut = petstore.api.petstore.listpets.Out_UEBACodec.decode(ctx, LEDataInputStream(ByteArrayInputStream(listBytes)))
    if (listOut.pets.size != 1) throw AssertionError("UEBA ListPets count: ${listOut.pets.size}")

    // DeletePet via UEBA
    val delIn = petstore.api.petstore.deletepet.In(id = id)
    val delBaos = ByteArrayOutputStream()
    petstore.api.petstore.deletepet.In_UEBACodec.encode(ctx, LEDataOutputStream(delBaos), delIn)
    val delBytes = PetStoreWiring.invokeUeba(BaboonMethodId("PetStore", "deletePet"), delBaos.toByteArray(), impl, ctx)
    val delOut = petstore.api.petstore.deletepet.Out_UEBACodec.decode(ctx, LEDataInputStream(ByteArrayInputStream(delBytes)))
    if (!delOut.deleted) throw AssertionError("UEBA DeletePet returned false")

    println("UEBA round-trip OK")
}

private suspend fun runJson(impl: PetStoreAsyncImpl, ctx: BaboonCodecContext) {
    // AddPet via JSON
    val addIn = petstore.api.petstore.addpet.In(name = "Whiskers", status = PetStatus.Pending, tag = "cat")
    val addJson = petstore.api.petstore.addpet.In_JsonCodec.encode(ctx, addIn).toString()
    val addResult = PetStoreWiring.invokeJson(BaboonMethodId("PetStore", "addPet"), addJson, impl, ctx)
    val addOut = petstore.api.petstore.addpet.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(addResult))
    if (addOut.pet.name != "Whiskers") throw AssertionError("JSON AddPet name mismatch: ${addOut.pet.name}")
    val id = addOut.pet.id

    // GetPet via JSON
    val getIn = petstore.api.petstore.getpet.In(id = id)
    val getJson = petstore.api.petstore.getpet.In_JsonCodec.encode(ctx, getIn).toString()
    val getResult = PetStoreWiring.invokeJson(BaboonMethodId("PetStore", "getPet"), getJson, impl, ctx)
    val getOut = petstore.api.petstore.getpet.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(getResult))
    if (getOut.pet.status != PetStatus.Pending) throw AssertionError("JSON GetPet status mismatch: ${getOut.pet.status}")

    // ListPets via JSON
    val listIn = petstore.api.petstore.listpets.In()
    val listJson = petstore.api.petstore.listpets.In_JsonCodec.encode(ctx, listIn).toString()
    val listResult = PetStoreWiring.invokeJson(BaboonMethodId("PetStore", "listPets"), listJson, impl, ctx)
    val listOut = petstore.api.petstore.listpets.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(listResult))
    if (listOut.pets.size != 1) throw AssertionError("JSON ListPets count: ${listOut.pets.size}")

    // DeletePet via JSON
    val delIn = petstore.api.petstore.deletepet.In(id = id)
    val delJson = petstore.api.petstore.deletepet.In_JsonCodec.encode(ctx, delIn).toString()
    val delResult = PetStoreWiring.invokeJson(BaboonMethodId("PetStore", "deletePet"), delJson, impl, ctx)
    val delOut = petstore.api.petstore.deletepet.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(delResult))
    if (!delOut.deleted) throw AssertionError("JSON DeletePet returned false")

    println("JSON round-trip OK")
}
