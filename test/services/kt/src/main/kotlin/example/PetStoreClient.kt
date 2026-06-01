package example

import kotlinx.coroutines.runBlocking
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.time.Duration

object PetStoreClient {
    fun run(host: String, port: Int) {
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

        val client = petstore.api.PetStoreClient { service, method, data ->
            post("/$service/$method", data)
        }

        runBlocking {
            // Reset
            post("/reset", "")

            // Add Buddy
            val addBuddyIn = petstore.api.petstore.addpet.In(
                name = "Buddy",
                status = petstore.api.PetStatus.Available,
                tag = "dog"
            )
            val addBuddyOut = client.addPetJson(addBuddyIn)
            val buddyId = addBuddyOut.pet.id
            assert(addBuddyOut.pet.name == "Buddy") { "expected name Buddy, got ${addBuddyOut.pet.name}" }

            // Add Whiskers
            val addWhiskersIn = petstore.api.petstore.addpet.In(
                name = "Whiskers",
                status = petstore.api.PetStatus.Pending,
                tag = "cat"
            )
            val addWhiskersOut = client.addPetJson(addWhiskersIn)
            val whiskersId = addWhiskersOut.pet.id
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

            println("OK")
        }
    }
}
