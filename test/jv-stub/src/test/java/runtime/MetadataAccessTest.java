package runtime;

import baboon.runtime.shared.*;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class MetadataAccessTest {
    public static class External implements BaboonGenerated {
        public static String baboonDomainIdentifier = "example";
        public static String baboonDomainVersion = "1.0.0";
        public static String baboonTypeIdentifier = "Branch";
        public static String baboonAdtTypeIdentifier = "Adt";
        public static List<String> baboonSameInVersions = List.of("1.0.0");
        public static Map<String, String> baboonMinReaderVersions = Map.of("json-additive", "1.0.0");
    }
    public static class Branch extends External implements BaboonAdtMemberMeta {}
    public static class Missing implements BaboonGenerated {}
    public static class Target implements BaboonGeneratedLatest {}
    public static class WrongString extends External { public static Object baboonDomainIdentifier = 7; }
    public static class WrongList extends External { public static Object baboonSameInVersions = "not-list"; }
    public static class WrongMap extends External { public static Object baboonMinReaderVersions = "not-map"; }
    public static class NonStatic extends External { public String baboonDomainIdentifier = "instance"; }

    @BeforeEach
    void resetExternalMetadata() {
        External.baboonDomainIdentifier = "example";
        External.baboonDomainVersion = "1.0.0";
        External.baboonSameInVersions = List.of("1.0.0");
        External.baboonMinReaderVersions = Map.of("json-additive", "1.0.0");
    }

    @Test
    void mutableExternalMetadataIsReadOnEveryCall() {
        External value = new External();
        assertEquals("1.0.0", BaboonExt.domainVersion(value).domainVersion());
        assertEquals("1.0.0", BaboonTypeMeta.from(value, External.class).domainVersion());
        External.baboonDomainVersion = "2.0.0";
        External.baboonSameInVersions = List.of("1.5.0", "2.0.0");
        External.baboonMinReaderVersions = Map.of("json-additive", "1.0.0");
        assertEquals("2.0.0", BaboonExt.domainVersion(value).domainVersion());
        assertEquals("1.5.0", BaboonExt.baboonUnmodifiedSinceVersion(value));
        BaboonTypeMeta meta = BaboonTypeMeta.from(value, External.class);
        assertEquals("2.0.0", meta.domainVersion());
        assertEquals("1.5.0", meta.domainVersionMinCompat());
        assertEquals("1.0.0", meta.domainVersionReadableMin());
    }

    @Test
    void declaredAdtTypeStillControlsEnvelopeIdentity() {
        Branch value = new Branch();
        assertEquals("Branch", BaboonTypeMeta.from(value, Branch.class).typeIdentifier());
        assertEquals("Adt", BaboonTypeMeta.from(value, BaboonAdtMemberMeta.class).typeIdentifier());
        assertEquals("Branch", BaboonTypeMeta.from(value, null).typeIdentifier());
    }

    @Test
    void malformedMetadataPreservesFailureCategoriesAndDiagnostics() {
        BaboonException missing = assertThrows(BaboonException.class, () -> BaboonExt.domainVersion(new Missing()));
        assertEquals("Type " + Missing.class.getName() + " is missing static field 'baboonDomainIdentifier'", missing.getMessage());
        assertInstanceOf(NoSuchFieldException.class, missing.getCause());
        BaboonException wrong = assertThrows(BaboonException.class, () -> BaboonExt.domainVersion(new WrongString()));
        assertEquals("Type " + WrongString.class.getName() + " field 'baboonDomainIdentifier' is not a String", wrong.getMessage());
        assertThrows(ClassCastException.class, () -> BaboonExt.baboonUnmodifiedSinceVersion(new WrongList()));
        assertThrows(ClassCastException.class, () -> BaboonTypeMeta.from(new WrongMap(), WrongMap.class));
        assertThrows(NullPointerException.class, () -> BaboonExt.domainVersion(new NonStatic()));
        External.baboonSameInVersions = List.of();
        assertThrows(IndexOutOfBoundsException.class, () -> BaboonTypeMeta.from(new External(), External.class));
        External.baboonSameInVersions = List.of("1.0.0");
        External.baboonMinReaderVersions = Map.of();
        assertThrows(BaboonException.class, () -> BaboonTypeMeta.from(new External(), External.class));
    }

    @Test
    void conversionRetainsItsEitherBoundaryAndUncheckedTypeFailures() {
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        var failure = facade.convert(new Missing(), Missing.class, Target.class);
        var left = assertInstanceOf(BaboonEither.Left.class, failure);
        var error = assertInstanceOf(BaboonCodecException.ConverterFailure.class, left.value());
        assertEquals("Cannot read baboon domain metadata from " + Missing.class.getName(), error.getMessage());
        assertInstanceOf(NoSuchFieldException.class, error.getCause());
        assertThrows(ClassCastException.class, () -> facade.convert(new WrongString(), WrongString.class, Target.class));
    }

    @Test
    void sameBinaryNameInDistinctClassLoadersDoesNotShareMetadata() throws Exception {
        Class<?> first = isolatedExternal();
        Class<?> second = isolatedExternal();
        first.getField("baboonDomainIdentifier").set(null, "first");
        second.getField("baboonDomainIdentifier").set(null, "second");
        assertNotSame(first, second);
        assertEquals("first", BaboonExt.domainVersion((BaboonGenerated) first.getConstructor().newInstance()).domainIdentifier());
        assertEquals("second", BaboonExt.domainVersion((BaboonGenerated) second.getConstructor().newInstance()).domainIdentifier());
    }

    private static Class<?> isolatedExternal() throws ClassNotFoundException {
        ClassLoader loader = new ClassLoader(MetadataAccessTest.class.getClassLoader()) {
            @Override
            protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
                if (!name.equals(External.class.getName())) return super.loadClass(name, resolve);
                try (var bytes = getParent().getResourceAsStream(name.replace('.', '/') + ".class")) {
                    if (bytes == null) throw new ClassNotFoundException(name);
                    byte[] code = bytes.readAllBytes();
                    Class<?> loaded = defineClass(name, code, 0, code.length);
                    if (resolve) resolveClass(loaded);
                    return loaded;
                } catch (IOException e) {
                    throw new ClassNotFoundException(name, e);
                }
            }
        };
        return loader.loadClass(External.class.getName());
    }
}
