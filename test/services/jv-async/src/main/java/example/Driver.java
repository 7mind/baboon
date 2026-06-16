package example;

import baboon.runtime.shared.BaboonClientTransport;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonMethodId;
import petstore.api.PetStatus;
import petstore.api.PetStoreClient;
import petstore.api.PetStoreWiring;

import java.util.Optional;

/**
 * In-process async round-trip driver for the Java async wiring lane.
 * Exercises the generated async PetStoreClient (async transport delegates)
 * end-to-end via both JSON and UEBA codecs, calling through the async
 * PetStoreWiring.invokeJson / invokeUeba dispatchers.
 * All assertions throw explicitly to avoid vacuous-assert problems.
 */
public final class Driver {

    private static void check(boolean condition, String message) {
        if (!condition) {
            throw new AssertionError(message);
        }
    }

    public static void main(String[] args) throws Exception {
        BaboonCodecContext ctx = BaboonCodecContext.Default;
        PetStoreAsyncImpl impl = new PetStoreAsyncImpl();

        BaboonClientTransport.UebaAsync uebaTransport = (svc, method, data) -> {
            try { return PetStoreWiring.invokeUeba(new BaboonMethodId(svc, method), data, impl, ctx); }
            catch (Exception e) { throw new RuntimeException(e); }
        };
        BaboonClientTransport.JsonAsync jsonTransport = (svc, method, data) -> {
            try { return PetStoreWiring.invokeJson(new BaboonMethodId(svc, method), data, impl, ctx); }
            catch (Exception e) { throw new RuntimeException(e); }
        };

        PetStoreClient client = new PetStoreClient(uebaTransport, jsonTransport, ctx);

        // ---- UEBA path ----
        impl.reset();
        var addOut = client.addPet(new petstore.api.petstore.addpet.In("Buddy", PetStatus.Available, Optional.of("dog"))).get();
        check(addOut.pet().name().equals("Buddy"), "UEBA AddPet name: " + addOut.pet().name());
        long id = addOut.pet().id();

        var getOut = client.getPet(new petstore.api.petstore.getpet.In(id)).get();
        check(getOut.pet().status() == PetStatus.Available, "UEBA GetPet status: " + getOut.pet().status());

        var listOut = client.listPets(new petstore.api.petstore.listpets.In()).get();
        check(listOut.pets().size() == 1, "UEBA ListPets count: " + listOut.pets().size());

        var delOut = client.deletePet(new petstore.api.petstore.deletepet.In(id)).get();
        check(delOut.deleted(), "UEBA DeletePet returned false");

        System.out.println("UEBA round-trip OK");

        // ---- JSON path ----
        impl.reset();
        var addOutJ = client.addPetJson(new petstore.api.petstore.addpet.In("Whiskers", PetStatus.Pending, Optional.of("cat"))).get();
        check(addOutJ.pet().name().equals("Whiskers"), "JSON AddPet name: " + addOutJ.pet().name());
        long idJ = addOutJ.pet().id();

        var getOutJ = client.getPetJson(new petstore.api.petstore.getpet.In(idJ)).get();
        check(getOutJ.pet().status() == PetStatus.Pending, "JSON GetPet status: " + getOutJ.pet().status());

        var listOutJ = client.listPetsJson(new petstore.api.petstore.listpets.In()).get();
        check(listOutJ.pets().size() == 1, "JSON ListPets count: " + listOutJ.pets().size());

        var delOutJ = client.deletePetJson(new petstore.api.petstore.deletepet.In(idJ)).get();
        check(delOutJ.deleted(), "JSON DeletePet returned false");

        System.out.println("JSON round-trip OK");
        System.out.println("OK");
    }
}
