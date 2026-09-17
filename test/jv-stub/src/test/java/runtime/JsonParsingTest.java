package runtime;

import baboon.runtime.shared.BaboonCodecContext;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class JsonParsingTest {
    @Test
    public void contextReaderPreservesDefaultMapperTreeSemantics() throws Exception {
        var mapper = new ObjectMapper();
        for (String text : new String[] { "", " ", "null", "true", "42", "1.234567890123456789", "\"é\"", "[1,null]", "{\"n\":1,\"n\":2}", "{} []" }) {
            assertEquals(mapper.readTree(text), BaboonCodecContext.Compact.parseJson(text), text);
            assertEquals(mapper.readTree(text), BaboonCodecContext.Indexed.parseJson(text), text);
        }
    }

    @Test
    public void contextReaderPreservesParseErrors() {
        for (String text : new String[] { "{", "[1,]", "undefined", "{\"a\":NaN}" }) {
            var expected = assertThrows(JsonProcessingException.class, () -> new ObjectMapper().readTree(text));
            var actual = assertThrows(JsonProcessingException.class, () -> BaboonCodecContext.Compact.parseJson(text));
            assertEquals(expected.getClass(), actual.getClass());
            assertEquals(expected.getOriginalMessage(), actual.getOriginalMessage());
            assertEquals(expected.getLocation().getCharOffset(), actual.getLocation().getCharOffset());
        }
    }
}
