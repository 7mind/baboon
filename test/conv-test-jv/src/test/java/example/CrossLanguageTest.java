package example;

import convtest.testpkg.AllBasicTypes;
import convtest.testpkg.AllBasicTypes_JsonCodec;
import convtest.testpkg.AllBasicTypes_UEBACodec;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.LEDataInputStream;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.io.FileInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

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
}
