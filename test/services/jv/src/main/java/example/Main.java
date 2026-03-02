package example;

public final class Main {
    private Main() {}

    public static void main(String[] args) throws Exception {
        String mode = null;
        String host = "127.0.0.1";
        int port = 18080;

        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "server", "client" -> mode = args[i];
                case "--host" -> {
                    if (i + 1 >= args.length) throw new IllegalArgumentException("--host requires a value");
                    host = args[++i];
                }
                case "--port" -> {
                    if (i + 1 >= args.length) throw new IllegalArgumentException("--port requires a value");
                    port = Integer.parseInt(args[++i]);
                }
                default -> throw new IllegalArgumentException("Unknown argument: " + args[i]);
            }
        }

        if (mode == null) throw new IllegalArgumentException("Mode (server|client) is required");

        switch (mode) {
            case "server" -> PetStoreServer.start(host, port);
            case "client" -> PetStoreClient.run(host, port);
            default -> throw new IllegalArgumentException("Unknown mode: " + mode);
        }
    }
}
