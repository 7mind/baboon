package example

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

object PetStoreClient {
  private val httpClient: HttpClient = HttpClient
    .newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .build()

  def run(host: String, port: Int): Unit = {
    val base = s"http://$host:$port"

    post(base, "/reset", "")

    val client = new petstore.api.PetStoreClient(
      (service, method, data) => post(base, s"/$service/$method", data)
    )

    // Add Buddy
    val addBuddyOut = client.addPetJson(
      petstore.api.petstore.addpet.In(
        name   = "Buddy",
        status = petstore.api.PetStatus.Available,
        tag    = Some("dog"),
      )
    )
    val buddyId = addBuddyOut.pet.id
    assert(addBuddyOut.pet.name == "Buddy", s"expected name Buddy, got ${addBuddyOut.pet.name}")

    // Add Whiskers
    val addWhiskersOut = client.addPetJson(
      petstore.api.petstore.addpet.In(
        name   = "Whiskers",
        status = petstore.api.PetStatus.Pending,
        tag    = Some("cat"),
      )
    )
    val whiskersId = addWhiskersOut.pet.id
    assert(addWhiskersOut.pet.name == "Whiskers", s"expected name Whiskers, got ${addWhiskersOut.pet.name}")

    // List pets (expect 2)
    val listOut = client.listPetsJson(petstore.api.petstore.listpets.In())
    assert(listOut.pets.size == 2, s"expected 2 pets, got ${listOut.pets.size}")

    // Get Buddy
    val getBuddyOut = client.getPetJson(petstore.api.petstore.getpet.In(id = buddyId))
    assert(getBuddyOut.pet.name == "Buddy", s"expected Buddy, got ${getBuddyOut.pet.name}")
    assert(getBuddyOut.pet.status == petstore.api.PetStatus.Available, s"expected Available, got ${getBuddyOut.pet.status}")
    assert(getBuddyOut.pet.tag.contains("dog"), s"expected tag dog, got ${getBuddyOut.pet.tag}")

    // Delete Whiskers
    val deleteOut = client.deletePetJson(petstore.api.petstore.deletepet.In(id = whiskersId))
    assert(deleteOut.deleted, s"expected deleted=true, got ${deleteOut.deleted}")

    // List pets again (expect 1)
    val list2Out = client.listPetsJson(petstore.api.petstore.listpets.In())
    assert(list2Out.pets.size == 1, s"expected 1 pet, got ${list2Out.pets.size}")
    assert(list2Out.pets.head.name == "Buddy", s"expected remaining pet Buddy, got ${list2Out.pets.head.name}")

    println("OK")
  }

  private def post(base: String, path: String, body: String): String = {
    val request = HttpRequest
      .newBuilder()
      .uri(URI.create(s"$base$path"))
      .expectContinue(false)
      .timeout(Duration.ofSeconds(10))
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .header("Content-Type", "application/json")
      .build()
    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    assert(response.statusCode() == 200, s"HTTP ${response.statusCode()}: ${response.body()}")
    response.body()
  }
}
