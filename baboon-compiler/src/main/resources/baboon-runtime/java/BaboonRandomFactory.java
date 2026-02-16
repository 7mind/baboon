package baboon.fixture;

import java.util.Random;

public final class BaboonRandomFactory {
    private BaboonRandomFactory() {}

    public static BaboonRandom defaultFactory() {
        return new BaboonRandomImpl(new Random());
    }
}
