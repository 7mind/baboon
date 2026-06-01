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

    public static void run(String host, int port, String codec) throws Exception {
        boolean runJson;
        boolean runUeba;
        switch (codec) {
            case "json" -> { runJson = true; runUeba = false; }
            case "ueba" -> { runJson = false; runUeba = true; }
            case "both" -> { runJson = true; runUeba = true; }
            default -> throw new IllegalArgumentException("Unknown --codec value: " + codec + " (expected json|ueba|both)");
        }

        HttpClient httpClient = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .build();

        BaboonClientTransport.JsonSync jsonTransport =
            (service, method, data) -> post(httpClient, host, port, "/" + service + "/" + method, data);
        BaboonClientTransport.UebaSync uebaTransport =
            (service, method, data) -> postBytes(httpClient, host, port, "/" + service + "/" + method, data);
        petstore.api.PetStoreClient client = new petstore.api.PetStoreClient(uebaTransport, jsonTransport);

        var addBuddyIn = new petstore.api.petstore.addpet.In("Buddy", PetStatus.Available, Optional.of("dog"));
        var addWhiskersIn = new petstore.api.petstore.addpet.In("Whiskers", PetStatus.Pending, Optional.of("cat"));
        var listIn = new petstore.api.petstore.listpets.In();

        // IDs captured from the JSON pass, used to cross-check the UEBA pass when both run.
        long buddyId = -1;
        long whiskersId = -1;

        if (runJson) {
            // Reset
            post(httpClient, host, port, "/reset", "");

            // Add Buddy
            var addBuddyOut = client.addPetJson(addBuddyIn);
            buddyId = addBuddyOut.pet().id();
            check(addBuddyOut.pet().name().equals("Buddy"), "expected name Buddy, got " + addBuddyOut.pet().name());

            // Add Whiskers
            var addWhiskersOut = client.addPetJson(addWhiskersIn);
            whiskersId = addWhiskersOut.pet().id();
            check(addWhiskersOut.pet().name().equals("Whiskers"), "expected name Whiskers, got " + addWhiskersOut.pet().name());

            // List pets (expect 2)
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
        }

        if (runUeba) {
            // ---- UEBA pass: identical scenario over binary transport ----
            // When the JSON pass also ran, cross-check identical results against it;
            // otherwise validate against the expected literals.
            post(httpClient, host, port, "/reset", "");

            var addBuddyOutU = client.addPet(addBuddyIn);
            long buddyIdU = addBuddyOutU.pet().id();
            if (runJson) {
                check(buddyIdU == buddyId, "UEBA buddy id mismatch: json=" + buddyId + " ueba=" + buddyIdU);
            }
            check(addBuddyOutU.pet().name().equals("Buddy"), "UEBA expected name Buddy, got " + addBuddyOutU.pet().name());

            var addWhiskersOutU = client.addPet(addWhiskersIn);
            long whiskersIdU = addWhiskersOutU.pet().id();
            if (runJson) {
                check(whiskersIdU == whiskersId, "UEBA whiskers id mismatch: json=" + whiskersId + " ueba=" + whiskersIdU);
            }
            check(addWhiskersOutU.pet().name().equals("Whiskers"), "UEBA expected name Whiskers, got " + addWhiskersOutU.pet().name());

            var listOutU = client.listPets(listIn);
            check(listOutU.pets().size() == 2, "UEBA expected 2 pets, got " + listOutU.pets().size());

            var getBuddyOutU = client.getPet(new petstore.api.petstore.getpet.In(buddyIdU));
            check(getBuddyOutU.pet().name().equals("Buddy"), "UEBA expected Buddy, got " + getBuddyOutU.pet().name());
            check(getBuddyOutU.pet().status() == PetStatus.Available, "UEBA expected Available, got " + getBuddyOutU.pet().status());
            check(getBuddyOutU.pet().tag().isPresent() && getBuddyOutU.pet().tag().get().equals("dog"), "UEBA expected tag dog, got " + getBuddyOutU.pet().tag());

            var deleteOutU = client.deletePet(new petstore.api.petstore.deletepet.In(whiskersIdU));
            check(deleteOutU.deleted(), "UEBA expected deleted=true, got false");

            var list2OutU = client.listPets(listIn);
            check(list2OutU.pets().size() == 1, "UEBA expected 1 pet, got " + list2OutU.pets().size());
            check(list2OutU.pets().get(0).name().equals("Buddy"), "UEBA expected remaining pet Buddy, got " + list2OutU.pets().get(0).name());
        }

        System.out.println("OK");
    }
}
