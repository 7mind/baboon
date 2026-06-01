package example

import kotlinx.coroutines.runBlocking
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.time.Duration

object PetStoreClient {
    fun run(host: String, port: Int, codec: String = "both") {
        val runJson = codec == "json" || codec == "both"
        val runUeba = codec == "ueba" || codec == "both"
        require(runJson || runUeba) { "Unknown codec: $codec (expected json|ueba|both)" }

        val httpClient = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .build()

        fun post(path: String, body: String): String {
            val request = HttpRequest.newBuilder()
                .uri(URI.create("http://$host:$port$path"))
                .expectContinue(false)
                .timeout(Duration.ofSeconds(10))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build()
            val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
            assert(response.statusCode() == 200) {
                "HTTP ${response.statusCode()}: ${response.body()}"
            }
            return response.body()
        }

        fun postBytes(path: String, body: ByteArray): ByteArray {
            val request = HttpRequest.newBuilder()
                .uri(URI.create("http://$host:$port$path"))
                .expectContinue(false)
                .timeout(Duration.ofSeconds(10))
                .header("Content-Type", "application/octet-stream")
                .POST(HttpRequest.BodyPublishers.ofByteArray(body))
                .build()
            val response = httpClient.send(request, HttpResponse.BodyHandlers.ofByteArray())
            assert(response.statusCode() == 200) {
                "HTTP ${response.statusCode()}: ${String(response.body(), Charsets.UTF_8)}"
            }
            return response.body()
        }

        val client = petstore.api.PetStoreClient(
            { service, method, data -> postBytes("/$service/$method", data) },
            { service, method, data -> post("/$service/$method", data) }
        )

        runBlocking {
            val addBuddyIn = petstore.api.petstore.addpet.In(
                name = "Buddy",
                status = petstore.api.PetStatus.Available,
                tag = "dog"
            )
            val addWhiskersIn = petstore.api.petstore.addpet.In(
                name = "Whiskers",
                status = petstore.api.PetStatus.Pending,
                tag = "cat"
            )

            var buddyId: Long? = null
            var whiskersId: Long? = null

            if (runJson) {
                // ---- JSON scenario ----
                // Reset
                post("/reset", "")

                // Add Buddy
                val addBuddyOut = client.addPetJson(addBuddyIn)
                buddyId = addBuddyOut.pet.id
                assert(addBuddyOut.pet.name == "Buddy") { "expected name Buddy, got ${addBuddyOut.pet.name}" }

                // Add Whiskers
                val addWhiskersOut = client.addPetJson(addWhiskersIn)
                whiskersId = addWhiskersOut.pet.id
                assert(addWhiskersOut.pet.name == "Whiskers") { "expected name Whiskers, got ${addWhiskersOut.pet.name}" }

                // List pets (expect 2)
                val listOut = client.listPetsJson(petstore.api.petstore.listpets.In())
                assert(listOut.pets.size == 2) { "expected 2 pets, got ${listOut.pets.size}" }

                // Get Buddy
                val getBuddyOut = client.getPetJson(petstore.api.petstore.getpet.In(id = buddyId))
                assert(getBuddyOut.pet.name == "Buddy") { "expected Buddy, got ${getBuddyOut.pet.name}" }
                assert(getBuddyOut.pet.status == petstore.api.PetStatus.Available) { "expected Available, got ${getBuddyOut.pet.status}" }
                assert(getBuddyOut.pet.tag == "dog") { "expected tag dog, got ${getBuddyOut.pet.tag}" }

                // Delete Whiskers
                val deleteOut = client.deletePetJson(petstore.api.petstore.deletepet.In(id = whiskersId))
                assert(deleteOut.deleted) { "expected deleted=true, got ${deleteOut.deleted}" }

                // List pets again (expect 1)
                val list2Out = client.listPetsJson(petstore.api.petstore.listpets.In())
                assert(list2Out.pets.size == 1) { "expected 1 pet, got ${list2Out.pets.size}" }
                assert(list2Out.pets[0].name == "Buddy") { "expected remaining pet Buddy, got ${list2Out.pets[0].name}" }
            }

            if (runUeba) {
                // ---- UEBA scenario ----
                // Same sequence over the binary transport; assert identical results.
                post("/reset", "")

                val addBuddyOutU = client.addPet(addBuddyIn)
                val buddyIdU = addBuddyOutU.pet.id
                assert(addBuddyOutU.pet.name == "Buddy") { "ueba: expected name Buddy, got ${addBuddyOutU.pet.name}" }
                if (buddyId != null) assert(buddyIdU == buddyId) { "ueba: expected buddyId $buddyId, got $buddyIdU" }

                val addWhiskersOutU = client.addPet(addWhiskersIn)
                val whiskersIdU = addWhiskersOutU.pet.id
                assert(addWhiskersOutU.pet.name == "Whiskers") { "ueba: expected name Whiskers, got ${addWhiskersOutU.pet.name}" }
                if (whiskersId != null) assert(whiskersIdU == whiskersId) { "ueba: expected whiskersId $whiskersId, got $whiskersIdU" }

                val listOutU = client.listPets(petstore.api.petstore.listpets.In())
                assert(listOutU.pets.size == 2) { "ueba: expected 2 pets, got ${listOutU.pets.size}" }

                val getBuddyOutU = client.getPet(petstore.api.petstore.getpet.In(id = buddyIdU))
                assert(getBuddyOutU.pet.name == "Buddy") { "ueba: expected Buddy, got ${getBuddyOutU.pet.name}" }
                assert(getBuddyOutU.pet.status == petstore.api.PetStatus.Available) { "ueba: expected Available, got ${getBuddyOutU.pet.status}" }
                assert(getBuddyOutU.pet.tag == "dog") { "ueba: expected tag dog, got ${getBuddyOutU.pet.tag}" }

                val deleteOutU = client.deletePet(petstore.api.petstore.deletepet.In(id = whiskersIdU))
                assert(deleteOutU.deleted) { "ueba: expected deleted=true, got ${deleteOutU.deleted}" }

                val list2OutU = client.listPets(petstore.api.petstore.listpets.In())
                assert(list2OutU.pets.size == 1) { "ueba: expected 1 pet, got ${list2OutU.pets.size}" }
                assert(list2OutU.pets[0].name == "Buddy") { "ueba: expected remaining pet Buddy, got ${list2OutU.pets[0].name}" }
            }

            println("OK")
        }
    }
}
