package example;

import convtest.testpkg.AllBasicTypes;
import convtest.testpkg.AllBasicTypes_JsonCodec;
import convtest.testpkg.AllBasicTypes_UEBACodec;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.ByteString;
import baboon.runtime.shared.LEDataOutputStream;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

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
        var sampleData = createSampleData();

        var baseDir = Path.of("../../target/compat-test").toAbsolutePath().normalize();
        var javaJsonDir = baseDir.resolve("java-json");
        var javaUebaDir = baseDir.resolve("java-ueba");

        Files.createDirectories(javaJsonDir);
        Files.createDirectories(javaUebaDir);

        var ctx = BaboonCodecContext.Default;

        JsonNode json = AllBasicTypes_JsonCodec.INSTANCE.encode(ctx, sampleData);
        var mapper = new ObjectMapper();
        var jsonStr = mapper.writeValueAsString(json);
        var jsonFile = javaJsonDir.resolve("all-basic-types.json");
        Files.writeString(jsonFile, jsonStr, StandardCharsets.UTF_8);
        System.out.println("Written JSON to " + jsonFile.toAbsolutePath());

        var baos = new ByteArrayOutputStream();
        var uebaWriter = new LEDataOutputStream(baos);
        try {
            AllBasicTypes_UEBACodec.INSTANCE.encode(ctx, uebaWriter, sampleData);
            uebaWriter.flush();
            var uebaBytes = baos.toByteArray();
            var uebaFile = javaUebaDir.resolve("all-basic-types.ueba");
            Files.write(uebaFile, uebaBytes);
            System.out.println("Written UEBA to " + uebaFile.toAbsolutePath());
        } finally {
            uebaWriter.close();
        }

        System.out.println("Java serialization complete!");
    }

    private static AllBasicTypes createSampleData() {
        return new AllBasicTypes(
            (byte) 42,             // vi8: i08 -> byte
            (short) 1234,          // vi16: i16 -> short
            123456,                // vi32: i32 -> int
            123456789L,            // vi64: i64 -> long
            (short) 200,           // vu8: u08 -> short
            50000,                 // vu16: u16 -> int
            3000000000L,           // vu32: u32 -> long
            10000000000L,          // vu64: u64 -> long
            3.14159f,              // vf32: f32 -> float
            2.718281828,           // vf64: f64 -> double
            new BigDecimal("123456789.987654321"),  // vf128: f128 -> BigDecimal
            "Hello, Baboon!",      // vstr: str -> String
            ByteString.of(new byte[]{0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73}),  // vbstr
            UUID.fromString("12345678-1234-5678-1234-567812345678"),  // vuid
            true,                  // vbit: bit -> boolean
            OffsetDateTime.of(2024, 6, 15, 12, 30, 45, 123456789, ZoneOffset.UTC),  // vtsu
            OffsetDateTime.of(2024, 6, 15, 14, 30, 45, 987654321, ZoneOffset.ofHours(2)),  // vtso
            Optional.of("optional value"),  // voptStr
            List.of(1, 2, 3, 4, 5),  // vlstI32
            Set.of("apple", "banana", "cherry"),  // vsetStr
            Map.of("one", 1, "two", 2, "three", 3),  // vmapStrI32
            Optional.of(List.of("nested", "list", "values")),  // voptLst
            List.of(Optional.of(10), Optional.empty(), Optional.of(20), Optional.of(30)),  // vlstOpt
            Map.of("numbers", List.of(1L, 2L, 3L), "more", List.of(4L, 5L, 6L))  // vmapLst
        );
    }
}
