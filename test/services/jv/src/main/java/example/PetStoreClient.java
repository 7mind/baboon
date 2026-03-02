package example;

import baboon.runtime.shared.BaboonCodecContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import petstore.api.PetStatus;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Optional;

public final class PetStoreClient {
    private static final BaboonCodecContext CTX = BaboonCodecContext.Default;
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private PetStoreClient() {}

    private static void check(boolean condition, String message) {
        if (!condition) {
            throw new AssertionError(message);
        }
    }

    private static String post(HttpClient client, String host, int port, String path, String body) throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create("http://" + host + ":" + port + path))
            .expectContinue(false)
            .timeout(Duration.ofSeconds(10))
            .header("Content-Type", "application/json")
            .POST(HttpRequest.BodyPublishers.ofString(body))
            .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        check(response.statusCode() == 200, "Expected 200, got " + response.statusCode() + ": " + response.body());
        return response.body();
    }

    public static void run(String host, int port) throws Exception {
        HttpClient client = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .build();
        {
            // Reset
            post(client, host, port, "/reset", "");

            // Add Buddy
            var addBuddyIn = new petstore.api.petstore.addpet.In("Buddy", PetStatus.Available, Optional.of("dog"));
            String addBuddyJson = petstore.api.petstore.addpet.petstore_addpet_In_JsonCodec.INSTANCE.encode(CTX, addBuddyIn).toString();
            String addBuddyResp = post(client, host, port, "/PetStore/addPet", addBuddyJson);
            JsonNode addBuddyNode = MAPPER.readTree(addBuddyResp);
            var addBuddyOut = petstore.api.petstore.addpet.petstore_addpet_Out_JsonCodec.INSTANCE.decode(CTX, addBuddyNode);
            long buddyId = addBuddyOut.pet().id();
            check(addBuddyOut.pet().name().equals("Buddy"), "expected name Buddy, got " + addBuddyOut.pet().name());

            // Add Whiskers
            var addWhiskersIn = new petstore.api.petstore.addpet.In("Whiskers", PetStatus.Pending, Optional.of("cat"));
            String addWhiskersJson = petstore.api.petstore.addpet.petstore_addpet_In_JsonCodec.INSTANCE.encode(CTX, addWhiskersIn).toString();
            String addWhiskersResp = post(client, host, port, "/PetStore/addPet", addWhiskersJson);
            JsonNode addWhiskersNode = MAPPER.readTree(addWhiskersResp);
            var addWhiskersOut = petstore.api.petstore.addpet.petstore_addpet_Out_JsonCodec.INSTANCE.decode(CTX, addWhiskersNode);
            long whiskersId = addWhiskersOut.pet().id();
            check(addWhiskersOut.pet().name().equals("Whiskers"), "expected name Whiskers, got " + addWhiskersOut.pet().name());

            // List pets (expect 2)
            var listIn = new petstore.api.petstore.listpets.In();
            String listJson = petstore.api.petstore.listpets.petstore_listpets_In_JsonCodec.INSTANCE.encode(CTX, listIn).toString();
            String listResp = post(client, host, port, "/PetStore/listPets", listJson);
            JsonNode listNode = MAPPER.readTree(listResp);
            var listOut = petstore.api.petstore.listpets.petstore_listpets_Out_JsonCodec.INSTANCE.decode(CTX, listNode);
            check(listOut.pets().size() == 2, "expected 2 pets, got " + listOut.pets().size());

            // Get Buddy
            var getBuddyIn = new petstore.api.petstore.getpet.In(buddyId);
            String getBuddyJson = petstore.api.petstore.getpet.petstore_getpet_In_JsonCodec.INSTANCE.encode(CTX, getBuddyIn).toString();
            String getBuddyResp = post(client, host, port, "/PetStore/getPet", getBuddyJson);
            JsonNode getBuddyNode = MAPPER.readTree(getBuddyResp);
            var getBuddyOut = petstore.api.petstore.getpet.petstore_getpet_Out_JsonCodec.INSTANCE.decode(CTX, getBuddyNode);
            check(getBuddyOut.pet().name().equals("Buddy"), "expected Buddy, got " + getBuddyOut.pet().name());
            check(getBuddyOut.pet().status() == PetStatus.Available, "expected Available, got " + getBuddyOut.pet().status());
            check(getBuddyOut.pet().tag().isPresent() && getBuddyOut.pet().tag().get().equals("dog"), "expected tag dog, got " + getBuddyOut.pet().tag());

            // Delete Whiskers
            var deleteIn = new petstore.api.petstore.deletepet.In(whiskersId);
            String deleteJson = petstore.api.petstore.deletepet.petstore_deletepet_In_JsonCodec.INSTANCE.encode(CTX, deleteIn).toString();
            String deleteResp = post(client, host, port, "/PetStore/deletePet", deleteJson);
            JsonNode deleteNode = MAPPER.readTree(deleteResp);
            var deleteOut = petstore.api.petstore.deletepet.petstore_deletepet_Out_JsonCodec.INSTANCE.decode(CTX, deleteNode);
            check(deleteOut.deleted(), "expected deleted=true, got false");

            // List pets again (expect 1)
            String list2Resp = post(client, host, port, "/PetStore/listPets", listJson);
            JsonNode list2Node = MAPPER.readTree(list2Resp);
            var list2Out = petstore.api.petstore.listpets.petstore_listpets_Out_JsonCodec.INSTANCE.decode(CTX, list2Node);
            check(list2Out.pets().size() == 1, "expected 1 pet, got " + list2Out.pets().size());
            check(list2Out.pets().get(0).name().equals("Buddy"), "expected remaining pet Buddy, got " + list2Out.pets().get(0).name());

            System.out.println("OK");
        }
    }
}
