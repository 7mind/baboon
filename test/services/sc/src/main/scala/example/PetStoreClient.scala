package example

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

object PetStoreClient {
  private val httpClient: HttpClient = HttpClient
    .newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .build()

  // Unconditional value-check: a plain `assert` can be elided by the Scala
  // compiler under `-Xdisable-assertions`, which would silently vacate the
  // acceptance scenario. This always throws so the checks stay live.
  private def check(cond: Boolean, msg: String): Unit = if (!cond) throw new RuntimeException(msg)

  def run(host: String, port: Int, codec: String): Unit = {
    val base = s"http://$host:$port"

    val client = new petstore.api.PetStoreClient(
      transportUeba = (service, method, data) => postBytes(base, s"/$service/$method", data),
      transportJson = (service, method, data) => post(base, s"/$service/$method", data),
    )

    if (codec == "json" || codec == "both") runJson(base, client)
    if (codec == "ueba" || codec == "both") runUeba(base, client)

    println("OK")
  }

  private def runJson(base: String, client: petstore.api.PetStoreClient): Unit = {
    post(base, "/reset", "")

    // Add Buddy
    val addBuddyOut = client.addPetJson(
      petstore.api.petstore.addpet.In(
        name   = "Buddy",
        status = petstore.api.PetStatus.Available,
        tag    = Some("dog"),
      )
    )
    val buddyId = addBuddyOut.pet.id
    check(addBuddyOut.pet.name == "Buddy", s"expected name Buddy, got ${addBuddyOut.pet.name}")

    // Add Whiskers
    val addWhiskersOut = client.addPetJson(
      petstore.api.petstore.addpet.In(
        name   = "Whiskers",
        status = petstore.api.PetStatus.Pending,
        tag    = Some("cat"),
      )
    )
    val whiskersId = addWhiskersOut.pet.id
    check(addWhiskersOut.pet.name == "Whiskers", s"expected name Whiskers, got ${addWhiskersOut.pet.name}")

    // List pets (expect 2)
    val listOut = client.listPetsJson(petstore.api.petstore.listpets.In())
    check(listOut.pets.size == 2, s"expected 2 pets, got ${listOut.pets.size}")

    // Get Buddy
    val getBuddyOut = client.getPetJson(petstore.api.petstore.getpet.In(id = buddyId))
    check(getBuddyOut.pet.name == "Buddy", s"expected Buddy, got ${getBuddyOut.pet.name}")
    check(getBuddyOut.pet.status == petstore.api.PetStatus.Available, s"expected Available, got ${getBuddyOut.pet.status}")
    check(getBuddyOut.pet.tag.contains("dog"), s"expected tag dog, got ${getBuddyOut.pet.tag}")

    // Delete Whiskers
    val deleteOut = client.deletePetJson(petstore.api.petstore.deletepet.In(id = whiskersId))
    check(deleteOut.deleted, s"expected deleted=true, got ${deleteOut.deleted}")

    // List pets again (expect 1)
    val list2Out = client.listPetsJson(petstore.api.petstore.listpets.In())
    check(list2Out.pets.size == 1, s"expected 1 pet, got ${list2Out.pets.size}")
    check(list2Out.pets.head.name == "Buddy", s"expected remaining pet Buddy, got ${list2Out.pets.head.name}")

    println("JSON OK")
  }

  // Same scenario over the generated UEBA client methods (bare names), driven by the
  // octet-stream transport. Asserts the UEBA path yields identical results to JSON.
  private def runUeba(base: String, client: petstore.api.PetStoreClient): Unit = {
    post(base, "/reset", "")

    // Add Buddy
    val addBuddyOut = client.addPet(
      petstore.api.petstore.addpet.In(
        name   = "Buddy",
        status = petstore.api.PetStatus.Available,
        tag    = Some("dog"),
      )
    )
    val buddyId = addBuddyOut.pet.id
    check(addBuddyOut.pet.name == "Buddy", s"[ueba] expected name Buddy, got ${addBuddyOut.pet.name}")

    // Add Whiskers
    val addWhiskersOut = client.addPet(
      petstore.api.petstore.addpet.In(
        name   = "Whiskers",
        status = petstore.api.PetStatus.Pending,
        tag    = Some("cat"),
      )
    )
    val whiskersId = addWhiskersOut.pet.id
    check(addWhiskersOut.pet.name == "Whiskers", s"[ueba] expected name Whiskers, got ${addWhiskersOut.pet.name}")

    // List pets (expect 2)
    val listOut = client.listPets(petstore.api.petstore.listpets.In())
    check(listOut.pets.size == 2, s"[ueba] expected 2 pets, got ${listOut.pets.size}")

    // Get Buddy
    val getBuddyOut = client.getPet(petstore.api.petstore.getpet.In(id = buddyId))
    check(getBuddyOut.pet.name == "Buddy", s"[ueba] expected Buddy, got ${getBuddyOut.pet.name}")
    check(getBuddyOut.pet.status == petstore.api.PetStatus.Available, s"[ueba] expected Available, got ${getBuddyOut.pet.status}")
    check(getBuddyOut.pet.tag.contains("dog"), s"[ueba] expected tag dog, got ${getBuddyOut.pet.tag}")

    // Delete Whiskers
    val deleteOut = client.deletePet(petstore.api.petstore.deletepet.In(id = whiskersId))
    check(deleteOut.deleted, s"[ueba] expected deleted=true, got ${deleteOut.deleted}")

    // List pets again (expect 1)
    val list2Out = client.listPets(petstore.api.petstore.listpets.In())
    check(list2Out.pets.size == 1, s"[ueba] expected 1 pet, got ${list2Out.pets.size}")
    check(list2Out.pets.head.name == "Buddy", s"[ueba] expected remaining pet Buddy, got ${list2Out.pets.head.name}")

    println("UEBA OK")
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

  private def postBytes(base: String, path: String, body: Array[Byte]): Array[Byte] = {
    val request = HttpRequest
      .newBuilder()
      .uri(URI.create(s"$base$path"))
      .expectContinue(false)
      .timeout(Duration.ofSeconds(10))
      .POST(HttpRequest.BodyPublishers.ofByteArray(body))
      .header("Content-Type", "application/octet-stream")
      .build()
    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofByteArray())
    assert(response.statusCode() == 200, s"HTTP ${response.statusCode()}: ${new String(response.body(), "UTF-8")}")
    response.body()
  }
}
