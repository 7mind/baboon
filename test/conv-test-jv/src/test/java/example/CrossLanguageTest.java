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
import baboon.runtime.shared.BaboonAnyOpaque;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.LEDataInputStream;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class CrossLanguageTest {

    private final Path baseDir = Path.of("../../target/compat-test").toAbsolutePath().normalize();
    private final BaboonCodecContext ctx = BaboonCodecContext.Default;
    private final ObjectMapper mapper = new ObjectMapper()
        .enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);

    private AllBasicTypes readJsonFile(String source) throws Exception {
        var file = baseDir.resolve(source + "-json/all-basic-types.json");
        var jsonStr = Files.readString(file, StandardCharsets.UTF_8);
        var json = mapper.readTree(jsonStr);
        return AllBasicTypes_JsonCodec.INSTANCE.decode(ctx, json);
    }

    private AllBasicTypes readUebaFile(String source) throws Exception {
        var file = baseDir.resolve(source + "-ueba/all-basic-types.ueba");
        try (var fis = new FileInputStream(file.toFile())) {
            var reader = new LEDataInputStream(fis);
            return AllBasicTypes_UEBACodec.INSTANCE.decode(ctx, reader);
        }
    }

    private void assertBasicFields(AllBasicTypes data, String label) {
        System.out.println("Successfully decoded " + label + ": " + data.vstr());
        assertEquals("Hello, Baboon!", data.vstr());
        assertEquals(123456, data.vi32());
        assertTrue(data.vbit());
    }

    @Test
    public void javaJsonShouldReadJavaGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("java"), "Java JSON");
    }

    @Test
    public void javaJsonShouldReadScalaGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("scala"), "Scala JSON");
    }

    @Test
    public void javaJsonShouldReadCsGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("cs"), "C# JSON");
    }

    @Test
    public void javaJsonShouldReadRustGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("rust"), "Rust JSON");
    }

    @Test
    public void javaJsonShouldReadPythonGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("python"), "Python JSON");
    }

    @Test
    public void javaJsonShouldReadTypeScriptGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("typescript"), "TypeScript JSON");
    }

    @Test
    public void javaJsonShouldReadKotlinGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("kotlin"), "Kotlin JSON");
    }

    @Test
    public void javaUebaShouldReadJavaGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("java"), "Java UEBA");
    }

    @Test
    public void javaUebaShouldReadScalaGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("scala"), "Scala UEBA");
    }

    @Test
    public void javaUebaShouldReadCsGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("cs"), "C# UEBA");
    }

    @Test
    public void javaUebaShouldReadRustGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("rust"), "Rust UEBA");
    }

    @Test
    public void javaUebaShouldReadPythonGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("python"), "Python UEBA");
    }

    @Test
    public void javaUebaShouldReadTypeScriptGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("typescript"), "TypeScript UEBA");
    }

    @Test
    public void javaUebaShouldReadKotlinGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("kotlin"), "Kotlin UEBA");
    }

    @Test
    public void crossLanguageJsonShouldProduceEquivalentData() throws Exception {
        var javaData = readJsonFile("java");
        var scalaData = readJsonFile("scala");
        var csData = readJsonFile("cs");
        var rustData = readJsonFile("rust");
        var tsData = readJsonFile("typescript");
        var kotlinData = readJsonFile("kotlin");

        assertEquals(javaData, scalaData, "Java and Scala JSON data should be equal");
        assertEquals(javaData, csData, "Java and C# JSON data should be equal");
        assertEquals(javaData, rustData, "Java and Rust JSON data should be equal");
        assertEquals(javaData, tsData, "Java and TypeScript JSON data should be equal");
        assertEquals(javaData, kotlinData, "Java and Kotlin JSON data should be equal");
    }

    @Test
    public void javaJsonShouldReadDartGeneratedJson() throws Exception {
        assertBasicFields(readJsonFile("dart"), "Dart JSON");
    }

    @Test
    public void javaUebaShouldReadDartGeneratedUeba() throws Exception {
        assertBasicFields(readUebaFile("dart"), "Dart UEBA");
    }

    @Test
    public void crossLanguageUebaShouldProduceEquivalentData() throws Exception {
        var javaData = readUebaFile("java");
        var scalaData = readUebaFile("scala");
        var csData = readUebaFile("cs");
        var rustData = readUebaFile("rust");
        var pythonData = readUebaFile("python");
        var tsData = readUebaFile("typescript");
        var kotlinData = readUebaFile("kotlin");

        assertEquals(javaData, scalaData, "Java and Scala UEBA data should be equal");
        assertEquals(javaData, csData, "Java and C# UEBA data should be equal");
        assertEquals(javaData, rustData, "Java and Rust UEBA data should be equal");
        assertEquals(javaData, pythonData, "Java and Python UEBA data should be equal");
        assertEquals(javaData, tsData, "Java and TypeScript UEBA data should be equal");
        assertEquals(javaData, kotlinData, "Java and Kotlin UEBA data should be equal");
    }

    @Test
    public void javaJsonShouldReadSwiftGeneratedJson() throws Exception {
        var file = baseDir.resolve("swift-json/all-basic-types.json");
        org.junit.jupiter.api.Assumptions.assumeTrue(Files.exists(file), "Swift JSON file not found, skipping");
        assertBasicFields(readJsonFile("swift"), "Swift JSON");
    }

    @Test
    public void javaUebaShouldReadSwiftGeneratedUeba() throws Exception {
        var file = baseDir.resolve("swift-ueba/all-basic-types.ueba");
        org.junit.jupiter.api.Assumptions.assumeTrue(Files.exists(file), "Swift UEBA file not found, skipping");
        assertBasicFields(readUebaFile("swift"), "Swift UEBA");
    }

    // -----------------------------------------------------------------------------
    // AnyShowcase cross-language tests (M13 / PR 13.2)
    // -----------------------------------------------------------------------------

    private static final List<InnerPayload> EXPECTED_ANY_PAYLOADS = List.of(
        new InnerPayload("variant-A", 1),
        new InnerPayload("variant-B", 2),
        new InnerPayload("variant-C", 3),
        new InnerPayload("variant-D1", 4),
        new InnerPayload("variant-D2", 5),
        new InnerPayload("variant-D3", 6),
        new InnerPayload("opt-any", 7),
        new InnerPayload("lst-any-0", 8)
    );

    private AnyShowcase readAnyShowcaseJson(String source) throws Exception {
        var file = baseDir.resolve(source + "-json/any-showcase.json");
        var jsonStr = Files.readString(file, StandardCharsets.UTF_8);
        var json = mapper.readTree(jsonStr);
        return AnyShowcase_JsonCodec.INSTANCE.decode(ctx, json);
    }

    private AnyShowcase readAnyShowcaseUeba(String source) throws Exception {
        var file = baseDir.resolve(source + "-ueba/any-showcase.ueba");
        try (var fis = new FileInputStream(file.toFile())) {
            var reader = new LEDataInputStream(fis);
            return AnyShowcase_UEBACodec.INSTANCE.decode(ctx, reader);
        }
    }

    private InnerPayload decodeInner(BaboonAnyOpaque.AnyOpaque o) throws Exception {
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

    private List<InnerPayload> decodeAllPayloads(AnyShowcase v) throws Exception {
        var optAny = v.optAny().orElseThrow(() -> new IllegalStateException("optAny was empty"));
        if (v.lstAny().isEmpty()) throw new IllegalStateException("lstAny was empty");
        var lst0 = v.lstAny().get(0);
        return List.of(
            decodeInner(v.vAnyA()),
            decodeInner(v.vAnyB()),
            decodeInner(v.vAnyC()),
            decodeInner(v.vAnyD1()),
            decodeInner(v.vAnyD2()),
            decodeInner(v.vAnyD3()),
            decodeInner(optAny),
            decodeInner(lst0)
        );
    }

    private void assertAnyShowcase(String source, String fmt, AnyShowcase v) throws Exception {
        var decoded = decodeAllPayloads(v);
        assertEquals(EXPECTED_ANY_PAYLOADS.size(), decoded.size(), source + " " + fmt + " payload count mismatch");
        for (int i = 0; i < EXPECTED_ANY_PAYLOADS.size(); i++) {
            assertEquals(EXPECTED_ANY_PAYLOADS.get(i), decoded.get(i),
                source + " " + fmt + " payload " + i + " mismatch");
        }
    }

    @Test public void anyShowcaseJavaJson() throws Exception   { assertAnyShowcase("java",   "JSON", readAnyShowcaseJson("java")); }
    @Test public void anyShowcaseJavaUeba() throws Exception   { assertAnyShowcase("java",   "UEBA", readAnyShowcaseUeba("java")); }
    @Test public void anyShowcaseScalaJson() throws Exception  { assertAnyShowcase("scala",  "JSON", readAnyShowcaseJson("scala")); }
    @Test public void anyShowcaseScalaUeba() throws Exception  { assertAnyShowcase("scala",  "UEBA", readAnyShowcaseUeba("scala")); }
    @Test public void anyShowcaseCsJson() throws Exception     { assertAnyShowcase("cs",     "JSON", readAnyShowcaseJson("cs")); }
    @Test public void anyShowcaseCsUeba() throws Exception     { assertAnyShowcase("cs",     "UEBA", readAnyShowcaseUeba("cs")); }
    @Test public void anyShowcaseRustJson() throws Exception   { assertAnyShowcase("rust",   "JSON", readAnyShowcaseJson("rust")); }
    @Test public void anyShowcaseRustUeba() throws Exception   { assertAnyShowcase("rust",   "UEBA", readAnyShowcaseUeba("rust")); }

    @Test public void anyShowcasePythonJson() throws Exception {
        var f = baseDir.resolve("python-json/any-showcase.json");
        Assumptions.assumeTrue(Files.exists(f), "python any-showcase JSON file not found, skipping");
        assertAnyShowcase("python", "JSON", readAnyShowcaseJson("python"));
    }
    @Test public void anyShowcasePythonUeba() throws Exception {
        var f = baseDir.resolve("python-ueba/any-showcase.ueba");
        Assumptions.assumeTrue(Files.exists(f), "python any-showcase UEBA file not found, skipping");
        assertAnyShowcase("python", "UEBA", readAnyShowcaseUeba("python"));
    }
    @Test public void anyShowcaseTypeScriptJson() throws Exception {
        var f = baseDir.resolve("typescript-json/any-showcase.json");
        Assumptions.assumeTrue(Files.exists(f), "typescript any-showcase JSON file not found, skipping");
        assertAnyShowcase("typescript", "JSON", readAnyShowcaseJson("typescript"));
    }
    @Test public void anyShowcaseTypeScriptUeba() throws Exception {
        var f = baseDir.resolve("typescript-ueba/any-showcase.ueba");
        Assumptions.assumeTrue(Files.exists(f), "typescript any-showcase UEBA file not found, skipping");
        assertAnyShowcase("typescript", "UEBA", readAnyShowcaseUeba("typescript"));
    }
    @Test public void anyShowcaseKotlinJson() throws Exception {
        var f = baseDir.resolve("kotlin-json/any-showcase.json");
        Assumptions.assumeTrue(Files.exists(f), "kotlin any-showcase JSON file not found, skipping");
        assertAnyShowcase("kotlin", "JSON", readAnyShowcaseJson("kotlin"));
    }
    @Test public void anyShowcaseKotlinUeba() throws Exception {
        var f = baseDir.resolve("kotlin-ueba/any-showcase.ueba");
        Assumptions.assumeTrue(Files.exists(f), "kotlin any-showcase UEBA file not found, skipping");
        assertAnyShowcase("kotlin", "UEBA", readAnyShowcaseUeba("kotlin"));
    }
    @Test public void anyShowcaseDartJson() throws Exception {
        var f = baseDir.resolve("dart-json/any-showcase.json");
        Assumptions.assumeTrue(Files.exists(f), "dart any-showcase JSON file not found, skipping");
        assertAnyShowcase("dart", "JSON", readAnyShowcaseJson("dart"));
    }
    @Test public void anyShowcaseDartUeba() throws Exception {
        var f = baseDir.resolve("dart-ueba/any-showcase.ueba");
        Assumptions.assumeTrue(Files.exists(f), "dart any-showcase UEBA file not found, skipping");
        assertAnyShowcase("dart", "UEBA", readAnyShowcaseUeba("dart"));
    }
    @Test public void anyShowcaseSwiftJson() throws Exception {
        var f = baseDir.resolve("swift-json/any-showcase.json");
        Assumptions.assumeTrue(Files.exists(f), "swift any-showcase JSON file not found, skipping");
        assertAnyShowcase("swift", "JSON", readAnyShowcaseJson("swift"));
    }
    @Test public void anyShowcaseSwiftUeba() throws Exception {
        var f = baseDir.resolve("swift-ueba/any-showcase.ueba");
        Assumptions.assumeTrue(Files.exists(f), "swift any-showcase UEBA file not found, skipping");
        assertAnyShowcase("swift", "UEBA", readAnyShowcaseUeba("swift"));
    }

    @Test
    public void anyShowcaseUebaByteIdenticalJavaScala() throws Exception {
        var javaBytes = Files.readAllBytes(baseDir.resolve("java-ueba/any-showcase.ueba"));
        var scalaBytes = Files.readAllBytes(baseDir.resolve("scala-ueba/any-showcase.ueba"));
        assertArrayEquals(javaBytes, scalaBytes, "Java and Scala UEBA bytes diverged");
    }

    // --------------------------------------------------------------------------------------------
    // PR-I.1b (M24 Phase 3.1) — Custom-foreign `<Foreign>_KeyCodec` extension hook (Java mirror)
    //
    // Mirrors the Scala reference test in Test_CrossLanguageCompat.scala: round-trip the Java-
    // emitted m24-foreign-keycodec.json through ForeignKeyHolder_JsonCodec and assert byte-
    // identity (PR-I-D02 pattern guidance) of the encoded JSON string against the canonical
    // wire form `{"m":{"alpha":"v1","beta":"v2"}}`.
    // --------------------------------------------------------------------------------------------

    @Test
    public void m24ForeignKeyCodecRoundTripJava() throws Exception {
        var file = baseDir.resolve("java-json/m24-foreign-keycodec.json");
        assertTrue(Files.exists(file), "Java m24-foreign-keycodec fixture not found: " + file);
        var jsonStr = Files.readString(file, StandardCharsets.UTF_8);
        var json = mapper.readTree(jsonStr);
        var decoded = convtest.m24foreign.ForeignKeyHolder_JsonCodec.INSTANCE.decode(ctx, json);
        var expectedMap = new java.util.LinkedHashMap<convtest.m24foreign.ItemKey, String>();
        expectedMap.put(new convtest.m24foreign.ItemKey("alpha"), "v1");
        expectedMap.put(new convtest.m24foreign.ItemKey("beta"), "v2");
        var expected = new convtest.m24foreign.ForeignKeyHolder(expectedMap);
        assertEquals(expected, decoded, "round-trip diverged");
    }

    @Test
    public void m24ForeignKeyCodecCanonicalWireForm() throws Exception {
        var sampleMap = new java.util.LinkedHashMap<convtest.m24foreign.ItemKey, String>();
        sampleMap.put(new convtest.m24foreign.ItemKey("alpha"), "v1");
        sampleMap.put(new convtest.m24foreign.ItemKey("beta"), "v2");
        var sample = new convtest.m24foreign.ForeignKeyHolder(sampleMap);
        var encoded = convtest.m24foreign.ForeignKeyHolder_JsonCodec.INSTANCE.encode(ctx, sample);
        var actual = mapper.writeValueAsString(encoded);
        var expected = "{\"m\":{\"alpha\":\"v1\",\"beta\":\"v2\"}}";
        assertEquals(expected, actual, "FStr_KeyCodec wire form diverged");
    }

    @Test
    public void anyShowcaseUebaByteIdenticalJavaCs() throws Exception {
        var javaBytes = Files.readAllBytes(baseDir.resolve("java-ueba/any-showcase.ueba"));
        var csBytes = Files.readAllBytes(baseDir.resolve("cs-ueba/any-showcase.ueba"));
        assertArrayEquals(javaBytes, csBytes, "Java and C# UEBA bytes diverged");
    }
}
