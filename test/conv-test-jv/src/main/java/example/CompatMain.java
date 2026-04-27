package example;

import convtest.testpkg.AllBasicTypes;
import convtest.testpkg.AllBasicTypes_JsonCodec;
import convtest.testpkg.AllBasicTypes_UEBACodec;
import convtest.testpkg.AnyShowcase;
import convtest.testpkg.AnyShowcase_JsonCodec;
import convtest.testpkg.AnyShowcase_UEBACodec;
import convtest.testpkg.InnerPayload;
import convtest.testpkg.InnerPayload_JsonCodec;
import convtest.testpkg.InnerPayload_UEBACodec;
import convtest.testpkg.BaboonCodecsJson;
import convtest.testpkg.BaboonCodecsUeba;
import baboon.runtime.shared.BaboonAnyOpaque;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonCodecsFacade;
import baboon.runtime.shared.BaboonDomainVersion;
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
    // Domain constants — match the convtest.testpkg domain at version 2.0.0.
    private static final String DOMAIN_ID = "convtest.testpkg";
    private static final String DOMAIN_VER = "2.0.0";
    private static final String INNER_TYPE_ID = "convtest.testpkg/:#InnerPayload";

    public static void main(String[] args) throws Exception {
        if (args.length >= 3 && args[0].equals("write")) {
            String outputDir = args[1];
            String format = args[2];
            new File(outputDir).mkdirs();
            var sampleData = createSampleData();
            var sampleAny = createSampleAnyShowcase();
            var ctx = BaboonCodecContext.Default;
            var facadeCtx = BaboonCodecContext.withFacade(false, freshFacade());
            switch (format) {
                case "json" -> {
                    writeJson(ctx, sampleData, outputDir);
                    writeJsonAny(facadeCtx, sampleAny, outputDir);
                }
                case "ueba" -> {
                    writeUeba(ctx, sampleData, outputDir);
                    writeUebaAny(facadeCtx, sampleAny, outputDir);
                }
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
        var sampleAny = createSampleAnyShowcase();
        var baseDir = Path.of("../../target/compat-test").toAbsolutePath().normalize();
        var javaJsonDir = baseDir.resolve("java-json");
        var javaUebaDir = baseDir.resolve("java-ueba");

        Files.createDirectories(javaJsonDir);
        Files.createDirectories(javaUebaDir);

        var ctx = BaboonCodecContext.Default;
        var facadeCtx = BaboonCodecContext.withFacade(false, freshFacade());
        writeJson(ctx, sampleData, javaJsonDir.toString());
        writeUeba(ctx, sampleData, javaUebaDir.toString());
        writeJsonAny(facadeCtx, sampleAny, javaJsonDir.toString());
        writeUebaAny(facadeCtx, sampleAny, javaUebaDir.toString());

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

    private static void writeJsonAny(BaboonCodecContext ctx, AnyShowcase data, String outputDir) throws Exception {
        var json = AnyShowcase_JsonCodec.INSTANCE.encode(ctx, data);
        var mapper = new ObjectMapper();
        var jsonStr = mapper.writeValueAsString(json);
        var path = Path.of(outputDir).resolve("any-showcase.json");
        Files.writeString(path, jsonStr, StandardCharsets.UTF_8);
        System.out.println("Written JSON to " + path.toAbsolutePath());
    }

    private static void writeUebaAny(BaboonCodecContext ctx, AnyShowcase data, String outputDir) throws Exception {
        var baos = new ByteArrayOutputStream();
        var out = new LEDataOutputStream(baos);
        try {
            AnyShowcase_UEBACodec.INSTANCE.encode(ctx, out, data);
            out.flush();
            var bytes = baos.toByteArray();
            var path = Path.of(outputDir).resolve("any-showcase.ueba");
            Files.write(path, bytes);
            System.out.println("Written UEBA to " + path.toAbsolutePath());
        } finally {
            out.close();
        }
    }

    private static BaboonCodecsFacade freshFacade() {
        var f = new BaboonCodecsFacade();
        f.register(
            new BaboonDomainVersion(DOMAIN_ID, DOMAIN_VER),
            BaboonCodecsJson::new,
            BaboonCodecsUeba::new
        );
        return f;
    }

    // Expected logical InnerPayload contents per AnyShowcase slot, in deterministic order:
    // [vAnyA, vAnyB, vAnyC, vAnyD1, vAnyD2, vAnyD3, optAny, lstAny[0]].
    // Must match the Scala/C# fixture exactly so cross-language reads produce the same payloads.
    private static List<InnerPayload> expectedInnerPayloads() {
        return List.of(
            new InnerPayload("variant-A", 1),
            new InnerPayload("variant-B", 2),
            new InnerPayload("variant-C", 3),
            new InnerPayload("variant-D1", 4),
            new InnerPayload("variant-D2", 5),
            new InnerPayload("variant-D3", 6),
            new InnerPayload("opt-any", 7),
            new InnerPayload("lst-any-0", 8)
        );
    }

    private static byte[] uebaBytes(InnerPayload p) throws Exception {
        var baos = new ByteArrayOutputStream();
        var out = new LEDataOutputStream(baos);
        try {
            InnerPayload_UEBACodec.INSTANCE.encode(BaboonCodecContext.Compact, out, p);
            out.flush();
        } finally {
            out.close();
        }
        return baos.toByteArray();
    }

    private static JsonNode asJson(InnerPayload p) {
        return InnerPayload_JsonCodec.INSTANCE.encode(BaboonCodecContext.Compact, p);
    }

    private static AnyShowcase createSampleAnyShowcase() throws Exception {
        var payloads = expectedInnerPayloads();
        var a = payloads.get(0); var b = payloads.get(1); var c = payloads.get(2);
        var d1 = payloads.get(3); var d2 = payloads.get(4); var d3 = payloads.get(5);
        var optP = payloads.get(6); var lstP = payloads.get(7);

        var metaA  = new BaboonAnyOpaque.AnyMeta((byte) 0x07, DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID);
        var metaB  = new BaboonAnyOpaque.AnyMeta((byte) 0x03, null, DOMAIN_VER, INNER_TYPE_ID);
        var metaC  = new BaboonAnyOpaque.AnyMeta((byte) 0x01, null, null, INNER_TYPE_ID);
        var metaD1 = new BaboonAnyOpaque.AnyMeta((byte) 0x06, DOMAIN_ID, DOMAIN_VER, null);
        var metaD2 = new BaboonAnyOpaque.AnyMeta((byte) 0x02, null, DOMAIN_VER, null);
        var metaD3 = new BaboonAnyOpaque.AnyMeta((byte) 0x00, null, null, null);
        var metaOpt = new BaboonAnyOpaque.AnyMeta((byte) 0x07, DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID);
        var metaLst = new BaboonAnyOpaque.AnyMeta((byte) 0x06, DOMAIN_ID, DOMAIN_VER, null);

        return new AnyShowcase(
            new BaboonAnyOpaque.AnyOpaqueJson(metaA,  asJson(a)),
            new BaboonAnyOpaque.AnyOpaqueJson(metaB,  asJson(b)),
            new BaboonAnyOpaque.AnyOpaqueJson(metaC,  asJson(c)),
            new BaboonAnyOpaque.AnyOpaqueUeba(metaD1, uebaBytes(d1)),
            new BaboonAnyOpaque.AnyOpaqueUeba(metaD2, uebaBytes(d2)),
            new BaboonAnyOpaque.AnyOpaqueUeba(metaD3, uebaBytes(d3)),
            Optional.of(new BaboonAnyOpaque.AnyOpaqueJson(metaOpt, asJson(optP))),
            List.of(new BaboonAnyOpaque.AnyOpaqueUeba(metaLst, uebaBytes(lstP)))
        );
    }

    private static InnerPayload decodeInner(BaboonAnyOpaque.AnyOpaque o) throws Exception {
        if (o instanceof BaboonAnyOpaque.AnyOpaqueUeba u) {
            var bais = new ByteArrayInputStream(u.bytes());
            var r = new LEDataInputStream(bais);
            try {
                return InnerPayload_UEBACodec.INSTANCE.decode(BaboonCodecContext.Compact, r);
            } finally {
                r.close();
            }
        } else if (o instanceof BaboonAnyOpaque.AnyOpaqueJson j) {
            return InnerPayload_JsonCodec.INSTANCE.decode(BaboonCodecContext.Compact, j.json());
        }
        throw new IllegalStateException("unexpected AnyOpaque subclass: " + o.getClass());
    }

    private static List<InnerPayload> decodeAllPayloads(AnyShowcase v) throws Exception {
        var optAny = v.optAny().orElseThrow(() -> new IllegalStateException("optAny was empty; expected non-null"));
        if (v.lstAny().isEmpty()) throw new IllegalStateException("lstAny was empty; expected one element");
        var lstFirst = v.lstAny().get(0);
        return List.of(
            decodeInner(v.vAnyA()),
            decodeInner(v.vAnyB()),
            decodeInner(v.vAnyC()),
            decodeInner(v.vAnyD1()),
            decodeInner(v.vAnyD2()),
            decodeInner(v.vAnyD3()),
            decodeInner(optAny),
            decodeInner(lstFirst)
        );
    }

    private static void readAndVerifyAnyShowcase(String filePath) {
        var ctx = BaboonCodecContext.Default;
        var file = Path.of(filePath);
        AnyShowcase data;
        try {
            if (filePath.endsWith(".json")) {
                var jsonStr = Files.readString(file, StandardCharsets.UTF_8);
                var mapper = new ObjectMapper();
                var jsonNode = mapper.readTree(jsonStr);
                data = AnyShowcase_JsonCodec.INSTANCE.decode(ctx, jsonNode);
            } else {
                var bytes = Files.readAllBytes(file);
                var bais = new ByteArrayInputStream(bytes);
                var r = new LEDataInputStream(bais);
                try {
                    data = AnyShowcase_UEBACodec.INSTANCE.decode(ctx, r);
                } finally {
                    r.close();
                }
            }
        } catch (Exception e) {
            System.err.println("AnyShowcase deserialization failed: " + e.getMessage());
            System.exit(1);
            return;
        }

        try {
            var expected = expectedInnerPayloads();
            var decoded = decodeAllPayloads(data);
            for (int i = 0; i < expected.size(); i++) {
                if (!expected.get(i).equals(decoded.get(i))) {
                    System.err.println("AnyShowcase payload " + i + " mismatch: expected " + expected.get(i) + ", got " + decoded.get(i));
                    System.exit(1);
                }
            }
        } catch (Exception e) {
            System.err.println("AnyShowcase decode failed: " + e.getMessage());
            System.exit(1);
        }
        System.out.println("OK");
    }

    private static void readAndVerify(String filePath) {
        if (filePath.endsWith("any-showcase.json") || filePath.endsWith("any-showcase.ueba")) {
            readAndVerifyAnyShowcase(filePath);
            return;
        }
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
