package example;

import baboon.runtime.shared.BaboonClientTransport;
import petstore.api.PetStatus;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Optional;

public final class ClientApp {
    private ClientApp() {}

    private static void check(boolean condition, String message) {
        if (!condition) {
            throw new AssertionError(message);
        }
    }

    private static String post(HttpClient httpClient, String host, int port, String path, String body) throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create("http://" + host + ":" + port + path))
            .expectContinue(false)
            .timeout(Duration.ofSeconds(10))
            .header("Content-Type", "application/json")
            .POST(HttpRequest.BodyPublishers.ofString(body))
            .build();
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        check(response.statusCode() == 200, "Expected 200, got " + response.statusCode() + ": " + response.body());
        return response.body();
    }

    private static byte[] postBytes(HttpClient httpClient, String host, int port, String path, byte[] body) throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create("http://" + host + ":" + port + path))
            .expectContinue(false)
            .timeout(Duration.ofSeconds(10))
            .header("Content-Type", "application/octet-stream")
            .POST(HttpRequest.BodyPublishers.ofByteArray(body))
            .build();
        HttpResponse<byte[]> response = httpClient.send(request, HttpResponse.BodyHandlers.ofByteArray());
        check(response.statusCode() == 200, "Expected 200, got " + response.statusCode()
            + ": " + new String(response.body(), java.nio.charset.StandardCharsets.UTF_8));
        return response.body();
    }

    public static void run(String host, int port) throws Exception {
        HttpClient httpClient = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .build();

        BaboonClientTransport.JsonSync jsonTransport =
            (service, method, data) -> post(httpClient, host, port, "/" + service + "/" + method, data);
        BaboonClientTransport.UebaSync uebaTransport =
            (service, method, data) -> postBytes(httpClient, host, port, "/" + service + "/" + method, data);
        petstore.api.PetStoreClient client = new petstore.api.PetStoreClient(uebaTransport, jsonTransport);

        // Reset
        post(httpClient, host, port, "/reset", "");

        // Add Buddy
        var addBuddyIn = new petstore.api.petstore.addpet.In("Buddy", PetStatus.Available, Optional.of("dog"));
        var addBuddyOut = client.addPetJson(addBuddyIn);
        long buddyId = addBuddyOut.pet().id();
        check(addBuddyOut.pet().name().equals("Buddy"), "expected name Buddy, got " + addBuddyOut.pet().name());

        // Add Whiskers
        var addWhiskersIn = new petstore.api.petstore.addpet.In("Whiskers", PetStatus.Pending, Optional.of("cat"));
        var addWhiskersOut = client.addPetJson(addWhiskersIn);
        long whiskersId = addWhiskersOut.pet().id();
        check(addWhiskersOut.pet().name().equals("Whiskers"), "expected name Whiskers, got " + addWhiskersOut.pet().name());

        // List pets (expect 2)
        var listIn = new petstore.api.petstore.listpets.In();
        var listOut = client.listPetsJson(listIn);
        check(listOut.pets().size() == 2, "expected 2 pets, got " + listOut.pets().size());

        // Get Buddy
        var getBuddyIn = new petstore.api.petstore.getpet.In(buddyId);
        var getBuddyOut = client.getPetJson(getBuddyIn);
        check(getBuddyOut.pet().name().equals("Buddy"), "expected Buddy, got " + getBuddyOut.pet().name());
        check(getBuddyOut.pet().status() == PetStatus.Available, "expected Available, got " + getBuddyOut.pet().status());
        check(getBuddyOut.pet().tag().isPresent() && getBuddyOut.pet().tag().get().equals("dog"), "expected tag dog, got " + getBuddyOut.pet().tag());

        // Delete Whiskers
        var deleteIn = new petstore.api.petstore.deletepet.In(whiskersId);
        var deleteOut = client.deletePetJson(deleteIn);
        check(deleteOut.deleted(), "expected deleted=true, got false");

        // List pets again (expect 1)
        var list2Out = client.listPetsJson(listIn);
        check(list2Out.pets().size() == 1, "expected 1 pet, got " + list2Out.pets().size());
        check(list2Out.pets().get(0).name().equals("Buddy"), "expected remaining pet Buddy, got " + list2Out.pets().get(0).name());

        // ---- UEBA pass: identical scenario over binary transport, assert identical results ----
        post(httpClient, host, port, "/reset", "");

        var addBuddyOutU = client.addPet(addBuddyIn);
        long buddyIdU = addBuddyOutU.pet().id();
        check(buddyIdU == buddyId, "UEBA buddy id mismatch: json=" + buddyId + " ueba=" + buddyIdU);
        check(addBuddyOutU.pet().name().equals(addBuddyOut.pet().name()), "UEBA buddy name mismatch");

        var addWhiskersOutU = client.addPet(addWhiskersIn);
        long whiskersIdU = addWhiskersOutU.pet().id();
        check(whiskersIdU == whiskersId, "UEBA whiskers id mismatch: json=" + whiskersId + " ueba=" + whiskersIdU);
        check(addWhiskersOutU.pet().name().equals(addWhiskersOut.pet().name()), "UEBA whiskers name mismatch");

        var listOutU = client.listPets(listIn);
        check(listOutU.pets().size() == listOut.pets().size(), "UEBA list size mismatch: json=" + listOut.pets().size() + " ueba=" + listOutU.pets().size());

        var getBuddyOutU = client.getPet(new petstore.api.petstore.getpet.In(buddyIdU));
        check(getBuddyOutU.pet().name().equals(getBuddyOut.pet().name()), "UEBA get name mismatch");
        check(getBuddyOutU.pet().status() == getBuddyOut.pet().status(), "UEBA get status mismatch");
        check(getBuddyOutU.pet().tag().equals(getBuddyOut.pet().tag()), "UEBA get tag mismatch: json=" + getBuddyOut.pet().tag() + " ueba=" + getBuddyOutU.pet().tag());

        var deleteOutU = client.deletePet(new petstore.api.petstore.deletepet.In(whiskersIdU));
        check(deleteOutU.deleted() == deleteOut.deleted(), "UEBA delete mismatch");

        var list2OutU = client.listPets(listIn);
        check(list2OutU.pets().size() == list2Out.pets().size(), "UEBA list2 size mismatch: json=" + list2Out.pets().size() + " ueba=" + list2OutU.pets().size());
        check(list2OutU.pets().get(0).name().equals(list2Out.pets().get(0).name()), "UEBA list2 remaining pet mismatch");

        System.out.println("OK");
    }
}
