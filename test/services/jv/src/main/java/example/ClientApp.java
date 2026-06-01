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

    public static void run(String host, int port) throws Exception {
        HttpClient httpClient = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .build();

        BaboonClientTransport.JsonSync transport =
            (service, method, data) -> post(httpClient, host, port, "/" + service + "/" + method, data);
        petstore.api.PetStoreClient client = new petstore.api.PetStoreClient(transport);

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

        System.out.println("OK");
    }
}
