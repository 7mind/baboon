package example

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

import baboon.runtime.shared.BaboonCodecContext

object PetStoreClient {
  private val ctx: BaboonCodecContext = BaboonCodecContext.Default
  private val httpClient: HttpClient = HttpClient
    .newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .build()

  def run(host: String, port: Int): Unit = {
    val base = s"http://$host:$port"

    post(base, "/reset", "")

    // Add Buddy
    val addBuddyIn = petstore.api.petstore.addpet.In(
      name   = "Buddy",
      status = petstore.api.PetStatus.Available,
      tag    = Some("dog"),
    )
    val addBuddyJson = petstore.api.petstore.addpet.In_JsonCodec.instance.encode(ctx, addBuddyIn).noSpaces
    val addBuddyResp = post(base, "/PetStore/addPet", addBuddyJson)
    val addBuddyOut  = decode(petstore.api.petstore.addpet.Out_JsonCodec.instance, addBuddyResp)
    val buddyId      = addBuddyOut.pet.id
    assert(addBuddyOut.pet.name == "Buddy", s"expected name Buddy, got ${addBuddyOut.pet.name}")

    // Add Whiskers
    val addWhiskersIn = petstore.api.petstore.addpet.In(
      name   = "Whiskers",
      status = petstore.api.PetStatus.Pending,
      tag    = Some("cat"),
    )
    val addWhiskersJson = petstore.api.petstore.addpet.In_JsonCodec.instance.encode(ctx, addWhiskersIn).noSpaces
    val addWhiskersResp = post(base, "/PetStore/addPet", addWhiskersJson)
    val addWhiskersOut  = decode(petstore.api.petstore.addpet.Out_JsonCodec.instance, addWhiskersResp)
    val whiskersId      = addWhiskersOut.pet.id
    assert(addWhiskersOut.pet.name == "Whiskers", s"expected name Whiskers, got ${addWhiskersOut.pet.name}")

    // List pets (expect 2)
    val listIn   = petstore.api.petstore.listpets.In()
    val listJson = petstore.api.petstore.listpets.In_JsonCodec.instance.encode(ctx, listIn).noSpaces
    val listResp = post(base, "/PetStore/listPets", listJson)
    val listOut  = decode(petstore.api.petstore.listpets.Out_JsonCodec.instance, listResp)
    assert(listOut.pets.size == 2, s"expected 2 pets, got ${listOut.pets.size}")

    // Get Buddy
    val getBuddyIn   = petstore.api.petstore.getpet.In(id = buddyId)
    val getBuddyJson = petstore.api.petstore.getpet.In_JsonCodec.instance.encode(ctx, getBuddyIn).noSpaces
    val getBuddyResp = post(base, "/PetStore/getPet", getBuddyJson)
    val getBuddyOut  = decode(petstore.api.petstore.getpet.Out_JsonCodec.instance, getBuddyResp)
    assert(getBuddyOut.pet.name == "Buddy", s"expected Buddy, got ${getBuddyOut.pet.name}")
    assert(getBuddyOut.pet.status == petstore.api.PetStatus.Available, s"expected Available, got ${getBuddyOut.pet.status}")
    assert(getBuddyOut.pet.tag.contains("dog"), s"expected tag dog, got ${getBuddyOut.pet.tag}")

    // Delete Whiskers
    val deleteIn   = petstore.api.petstore.deletepet.In(id = whiskersId)
    val deleteJson = petstore.api.petstore.deletepet.In_JsonCodec.instance.encode(ctx, deleteIn).noSpaces
    val deleteResp = post(base, "/PetStore/deletePet", deleteJson)
    val deleteOut  = decode(petstore.api.petstore.deletepet.Out_JsonCodec.instance, deleteResp)
    assert(deleteOut.deleted, s"expected deleted=true, got ${deleteOut.deleted}")

    // List pets again (expect 1)
    val list2Resp = post(base, "/PetStore/listPets", listJson)
    val list2Out  = decode(petstore.api.petstore.listpets.Out_JsonCodec.instance, list2Resp)
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

  private def decode[T](codec: baboon.runtime.shared.BaboonJsonCodec[T], responseBody: String): T = {
    val json = io.circe.parser.parse(responseBody) match {
      case Right(j) => j
      case Left(e)  => throw new RuntimeException(s"Failed to parse JSON: $e")
    }
    codec.decode(ctx, json) match {
      case Right(v) => v
      case Left(e)  => throw new RuntimeException(s"Failed to decode: $e")
    }
  }
}
