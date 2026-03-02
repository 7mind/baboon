package example

import baboon.runtime.shared.BaboonCodecContext
import kotlinx.serialization.json.Json
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.time.Duration

object PetStoreClient {
    fun run(host: String, port: Int) {
        val client = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .build()
        val ctx = BaboonCodecContext.Default

        fun post(path: String, body: String): String {
            val request = HttpRequest.newBuilder()
                .uri(URI.create("http://$host:$port$path"))
                .expectContinue(false)
                .timeout(Duration.ofSeconds(10))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build()
            val response = client.send(request, HttpResponse.BodyHandlers.ofString())
            assert(response.statusCode() == 200) {
                "HTTP ${response.statusCode()}: ${response.body()}"
            }
            return response.body()
        }

        // Reset
        post("/reset", "")

        // Add Buddy
        val addBuddyIn = petstore.api.petstore.addpet.In(
            name = "Buddy",
            status = petstore.api.PetStatus.Available,
            tag = "dog"
        )
        val addBuddyJson = petstore.api.petstore.addpet.In_JsonCodec.encode(ctx, addBuddyIn).toString()
        val addBuddyResp = post("/PetStore/addPet", addBuddyJson)
        val addBuddyOut = petstore.api.petstore.addpet.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(addBuddyResp))
        val buddyId = addBuddyOut.pet.id
        assert(addBuddyOut.pet.name == "Buddy") { "expected name Buddy, got ${addBuddyOut.pet.name}" }

        // Add Whiskers
        val addWhiskersIn = petstore.api.petstore.addpet.In(
            name = "Whiskers",
            status = petstore.api.PetStatus.Pending,
            tag = "cat"
        )
        val addWhiskersJson = petstore.api.petstore.addpet.In_JsonCodec.encode(ctx, addWhiskersIn).toString()
        val addWhiskersResp = post("/PetStore/addPet", addWhiskersJson)
        val addWhiskersOut = petstore.api.petstore.addpet.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(addWhiskersResp))
        val whiskersId = addWhiskersOut.pet.id
        assert(addWhiskersOut.pet.name == "Whiskers") { "expected name Whiskers, got ${addWhiskersOut.pet.name}" }

        // List pets (expect 2)
        val listIn = petstore.api.petstore.listpets.In()
        val listJson = petstore.api.petstore.listpets.In_JsonCodec.encode(ctx, listIn).toString()
        val listResp = post("/PetStore/listPets", listJson)
        val listOut = petstore.api.petstore.listpets.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(listResp))
        assert(listOut.pets.size == 2) { "expected 2 pets, got ${listOut.pets.size}" }

        // Get Buddy
        val getBuddyIn = petstore.api.petstore.getpet.In(id = buddyId)
        val getBuddyJson = petstore.api.petstore.getpet.In_JsonCodec.encode(ctx, getBuddyIn).toString()
        val getBuddyResp = post("/PetStore/getPet", getBuddyJson)
        val getBuddyOut = petstore.api.petstore.getpet.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(getBuddyResp))
        assert(getBuddyOut.pet.name == "Buddy") { "expected Buddy, got ${getBuddyOut.pet.name}" }
        assert(getBuddyOut.pet.status == petstore.api.PetStatus.Available) { "expected Available, got ${getBuddyOut.pet.status}" }
        assert(getBuddyOut.pet.tag == "dog") { "expected tag dog, got ${getBuddyOut.pet.tag}" }

        // Delete Whiskers
        val deleteIn = petstore.api.petstore.deletepet.In(id = whiskersId)
        val deleteJson = petstore.api.petstore.deletepet.In_JsonCodec.encode(ctx, deleteIn).toString()
        val deleteResp = post("/PetStore/deletePet", deleteJson)
        val deleteOut = petstore.api.petstore.deletepet.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(deleteResp))
        assert(deleteOut.deleted) { "expected deleted=true, got ${deleteOut.deleted}" }

        // List pets again (expect 1)
        val list2Resp = post("/PetStore/listPets", listJson)
        val list2Out = petstore.api.petstore.listpets.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(list2Resp))
        assert(list2Out.pets.size == 1) { "expected 1 pet, got ${list2Out.pets.size}" }
        assert(list2Out.pets[0].name == "Buddy") { "expected remaining pet Buddy, got ${list2Out.pets[0].name}" }

        println("OK")
    }
}
