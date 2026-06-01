package example;

import baboon.runtime.shared.BaboonClientTransport;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonMethodId;
import petstore.api.PetStatus;
import petstore.api.PetStoreClient;
import petstore.api.PetStoreWiring;

import java.util.Optional;

/**
 * In-process round-trip for the GENERATED {@link PetStoreClient}:
 *
 *   client encode -> in-process transport -> server PetStoreWiring.invoke* -> decode
 *
 * Exercises both the JSON-suffixed client methods (transportJson +
 * invokeJson) and the UEBA default-named client methods (transportUeba +
 * invokeUeba). The transport callbacks are pure in-process dispatch into
 * the server wiring against a shared {@link PetStoreImpl}; no sockets.
 */
public final class ClientRoundTrip {
    private ClientRoundTrip() {}

    private static void check(boolean condition, String message) {
        if (!condition) {
            throw new AssertionError(message);
        }
    }

    public static void main(String[] args) throws Exception {
        BaboonCodecContext ctx = BaboonCodecContext.Default;
        PetStoreImpl impl = new PetStoreImpl();

        BaboonClientTransport.JsonSync jsonTransport =
            (service, method, data) -> PetStoreWiring.invokeJson(new BaboonMethodId(service, method), data, impl, ctx);
        BaboonClientTransport.UebaSync uebaTransport =
            (service, method, data) -> PetStoreWiring.invokeUeba(new BaboonMethodId(service, method), data, impl, ctx);

        // The generated sync client takes both transports + ctx.
        PetStoreClient client = new PetStoreClient(uebaTransport, jsonTransport, ctx);

        // ---- JSON path (the *Json client methods) ----
        impl.reset();
        runScenario("JSON", client, true);

        // ---- UEBA path (the bare-named client methods) ----
        impl.reset();
        runScenario("UEBA", client, false);

        System.out.println("OK");
    }

    private static void runScenario(String label, PetStoreClient client, boolean json) throws Exception {
        var addBuddyIn = new petstore.api.petstore.addpet.In("Buddy", PetStatus.Available, Optional.of("dog"));
        var addBuddyOut = json ? client.addPetJson(addBuddyIn) : client.addPet(addBuddyIn);
        long buddyId = addBuddyOut.pet().id();
        check(addBuddyOut.pet().name().equals("Buddy"), label + ": expected name Buddy, got " + addBuddyOut.pet().name());

        var addWhiskersIn = new petstore.api.petstore.addpet.In("Whiskers", PetStatus.Pending, Optional.of("cat"));
        var addWhiskersOut = json ? client.addPetJson(addWhiskersIn) : client.addPet(addWhiskersIn);
        long whiskersId = addWhiskersOut.pet().id();
        check(addWhiskersOut.pet().name().equals("Whiskers"), label + ": expected name Whiskers, got " + addWhiskersOut.pet().name());

        var listIn = new petstore.api.petstore.listpets.In();
        var listOut = json ? client.listPetsJson(listIn) : client.listPets(listIn);
        check(listOut.pets().size() == 2, label + ": expected 2 pets, got " + listOut.pets().size());

        var getBuddyIn = new petstore.api.petstore.getpet.In(buddyId);
        var getBuddyOut = json ? client.getPetJson(getBuddyIn) : client.getPet(getBuddyIn);
        check(getBuddyOut.pet().name().equals("Buddy"), label + ": expected Buddy, got " + getBuddyOut.pet().name());
        check(getBuddyOut.pet().status() == PetStatus.Available, label + ": expected Available, got " + getBuddyOut.pet().status());
        check(getBuddyOut.pet().tag().isPresent() && getBuddyOut.pet().tag().get().equals("dog"),
            label + ": expected tag dog, got " + getBuddyOut.pet().tag());

        var deleteIn = new petstore.api.petstore.deletepet.In(whiskersId);
        var deleteOut = json ? client.deletePetJson(deleteIn) : client.deletePet(deleteIn);
        check(deleteOut.deleted(), label + ": expected deleted=true, got false");

        var list2Out = json ? client.listPetsJson(listIn) : client.listPets(listIn);
        check(list2Out.pets().size() == 1, label + ": expected 1 pet, got " + list2Out.pets().size());
        check(list2Out.pets().get(0).name().equals("Buddy"), label + ": expected remaining pet Buddy, got " + list2Out.pets().get(0).name());

        System.out.println(label + " round-trip OK");
    }
}
