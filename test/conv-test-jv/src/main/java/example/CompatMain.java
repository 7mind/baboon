package example;

import convtest.testpkg.AllBasicTypes;
import convtest.testpkg.AllBasicTypes_JsonCodec;
import convtest.testpkg.AllBasicTypes_UEBACodec;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.ByteString;
import baboon.runtime.shared.LEDataInputStream;
import baboon.runtime.shared.LEDataOutputStream;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public class CompatMain {
    public static void main(String[] args) throws Exception {
        if (args.length >= 3 && args[0].equals("write")) {
            String outputDir = args[1];
            String format = args[2];
            new File(outputDir).mkdirs();
            var sampleData = createSampleData();
            var ctx = BaboonCodecContext.Default;
            switch (format) {
                case "json" -> writeJson(ctx, sampleData, outputDir);
                case "ueba" -> writeUeba(ctx, sampleData, outputDir);
                default -> {
                    System.err.println("Unknown format: " + format);
                    System.exit(1);
                }
            }
        } else if (args.length >= 2 && args[0].equals("read")) {
            readAndVerify(args[1]);
        } else {
            runLegacy();
        }
    }

    private static void runLegacy() throws Exception {
        var sampleData = createSampleData();
        var baseDir = Path.of("../../target/compat-test").toAbsolutePath().normalize();
        var javaJsonDir = baseDir.resolve("java-json");
        var javaUebaDir = baseDir.resolve("java-ueba");

        Files.createDirectories(javaJsonDir);
        Files.createDirectories(javaUebaDir);

        var ctx = BaboonCodecContext.Default;
        writeJson(ctx, sampleData, javaJsonDir.toString());
        writeUeba(ctx, sampleData, javaUebaDir.toString());

        System.out.println("Java serialization complete!");
    }

    private static void writeJson(BaboonCodecContext ctx, AllBasicTypes data, String outputDir) throws Exception {
        JsonNode json = AllBasicTypes_JsonCodec.INSTANCE.encode(ctx, data);
        var mapper = new ObjectMapper();
        var jsonStr = mapper.writeValueAsString(json);
        var jsonFile = Path.of(outputDir).resolve("all-basic-types.json");
        Files.writeString(jsonFile, jsonStr, StandardCharsets.UTF_8);
        System.out.println("Written JSON to " + jsonFile.toAbsolutePath());
    }

    private static void writeUeba(BaboonCodecContext ctx, AllBasicTypes data, String outputDir) throws Exception {
        var baos = new ByteArrayOutputStream();
        var uebaWriter = new LEDataOutputStream(baos);
        try {
            AllBasicTypes_UEBACodec.INSTANCE.encode(ctx, uebaWriter, data);
            uebaWriter.flush();
            var uebaBytes = baos.toByteArray();
            var uebaFile = Path.of(outputDir).resolve("all-basic-types.ueba");
            Files.write(uebaFile, uebaBytes);
            System.out.println("Written UEBA to " + uebaFile.toAbsolutePath());
        } finally {
            uebaWriter.close();
        }
    }

    private static void readAndVerify(String filePath) {
        var ctx = BaboonCodecContext.Default;
        var file = Path.of(filePath);
        AllBasicTypes data;

        try {
            if (filePath.endsWith(".json")) {
                var jsonStr = Files.readString(file, StandardCharsets.UTF_8);
                var mapper = new ObjectMapper();
                var jsonNode = mapper.readTree(jsonStr);
                data = AllBasicTypes_JsonCodec.INSTANCE.decode(ctx, jsonNode);
            } else if (filePath.endsWith(".ueba")) {
                var bytes = Files.readAllBytes(file);
                var bais = new ByteArrayInputStream(bytes);
                var uebaReader = new LEDataInputStream(bais);
                try {
                    data = AllBasicTypes_UEBACodec.INSTANCE.decode(ctx, uebaReader);
                } finally {
                    uebaReader.close();
                }
            } else {
                System.err.println("Unknown file extension: " + filePath);
                System.exit(1);
                return;
            }
        } catch (Exception e) {
            System.err.println("Deserialization failed: " + e.getMessage());
            System.exit(1);
            return;
        }

        if (!data.vstr().equals("Hello, Baboon!")) {
            System.err.println("vstr mismatch: expected 'Hello, Baboon!', got '" + data.vstr() + "'");
            System.exit(1);
        }
        if (data.vi32() != 123456) {
            System.err.println("vi32 mismatch: expected 123456, got " + data.vi32());
            System.exit(1);
        }
        if (!data.vbit()) {
            System.err.println("vbit mismatch: expected true, got " + data.vbit());
            System.exit(1);
        }

        // Roundtrip
        try {
            if (filePath.endsWith(".json")) {
                var reEncoded = AllBasicTypes_JsonCodec.INSTANCE.encode(ctx, data);
                var reDecoded = AllBasicTypes_JsonCodec.INSTANCE.decode(ctx, reEncoded);
                if (!data.equals(reDecoded)) {
                    System.err.println("JSON roundtrip mismatch");
                    System.exit(1);
                }
            } else {
                var baos = new ByteArrayOutputStream();
                var w = new LEDataOutputStream(baos);
                try {
                    AllBasicTypes_UEBACodec.INSTANCE.encode(ctx, w, data);
                    w.flush();
                } finally {
                    w.close();
                }
                var reBytes = baos.toByteArray();
                var bais = new ByteArrayInputStream(reBytes);
                var r = new LEDataInputStream(bais);
                try {
                    var reDecoded = AllBasicTypes_UEBACodec.INSTANCE.decode(ctx, r);
                    if (!data.equals(reDecoded)) {
                        System.err.println("UEBA roundtrip mismatch");
                        System.exit(1);
                    }
                } finally {
                    r.close();
                }
            }
        } catch (Exception e) {
            System.err.println("Roundtrip failed: " + e.getMessage());
            System.exit(1);
        }

        System.out.println("OK");
    }

    private static AllBasicTypes createSampleData() {
        return new AllBasicTypes(
            (byte) 42,
            (short) 1234,
            123456,
            123456789L,
            (short) 200,
            50000,
            3000000000L,
            10000000000L,
            3.14159f,
            2.718281828,
            new BigDecimal("123456789.987654321"),
            "Hello, Baboon!",
            ByteString.of(new byte[]{0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73}),
            UUID.fromString("12345678-1234-5678-1234-567812345678"),
            true,
            OffsetDateTime.of(2024, 6, 15, 12, 30, 45, 123456789, ZoneOffset.UTC),
            OffsetDateTime.of(2024, 6, 15, 14, 30, 45, 987654321, ZoneOffset.ofHours(2)),
            Optional.of("optional value"),
            List.of(1, 2, 3, 4, 5),
            Set.of("apple", "banana", "cherry"),
            Map.of("one", 1, "two", 2, "three", 3),
            Optional.of(List.of("nested", "list", "values")),
            List.of(Optional.of(10), Optional.empty(), Optional.of(20), Optional.of(30)),
            Map.of("numbers", List.of(1L, 2L, 3L), "more", List.of(4L, 5L, 6L))
        );
    }
}
