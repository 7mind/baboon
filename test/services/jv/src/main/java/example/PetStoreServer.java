package example;

import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonMethodId;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import petstore.api.PetStoreWiring;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;

public final class PetStoreServer {
    private PetStoreServer() {}

    public static void start(String host, int port) throws Exception {
        PetStoreImpl impl = new PetStoreImpl();
        HttpServer server = HttpServer.create(new InetSocketAddress(host, port), 0);
        CountDownLatch shutdownLatch = new CountDownLatch(1);

        server.createContext("/health", exchange -> {
            sendText(exchange, 200, "ok");
        });

        server.createContext("/reset", exchange -> {
            impl.reset();
            sendText(exchange, 200, "ok");
        });

        server.createContext("/shutdown", exchange -> {
            if (!"POST".equals(exchange.getRequestMethod())) {
                sendText(exchange, 405, "Method Not Allowed");
                return;
            }
            sendText(exchange, 200, "ok");
            Thread stopper = new Thread(() -> {
                server.stop(0);
                shutdownLatch.countDown();
            });
            stopper.setDaemon(false);
            stopper.start();
        });

        server.createContext("/", exchange -> {
            String path = exchange.getRequestURI().getPath();
            String[] parts = path.split("/");
            // parts[0] is empty (leading slash), parts[1] is service, parts[2] is method
            if (parts.length == 3) {
                String service = parts[1];
                String method = parts[2];
                try {
                    byte[] bodyBytes = exchange.getRequestBody().readAllBytes();
                    String body = new String(bodyBytes, StandardCharsets.UTF_8);
                    String result = PetStoreWiring.invokeJson(
                        new BaboonMethodId(service, method),
                        body,
                        impl,
                        BaboonCodecContext.Default
                    );
                    sendJson(exchange, 200, result);
                } catch (Exception e) {
                    sendText(exchange, 500, e.getMessage());
                }
            } else {
                sendText(exchange, 404, "Not Found");
            }
        });

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            server.stop(0);
            shutdownLatch.countDown();
        }));

        server.start();
        System.out.println("Listening on " + host + ":" + port);
        System.out.flush();

        shutdownLatch.await();
    }

    private static void sendText(HttpExchange exchange, int status, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "text/plain");
        exchange.sendResponseHeaders(status, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    private static void sendJson(HttpExchange exchange, int status, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }
}
