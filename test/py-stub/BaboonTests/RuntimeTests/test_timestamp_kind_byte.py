# docs/ueba-format.md: the trailing `i8` kind byte of a `tso`/`tsu` value is a pure function of the
# offset -- 1 when the offset is zero, 0 otherwise -- and never the writer's environment
# (https://github.com/7mind/baboon/issues/91).
import unittest
from datetime import datetime, timedelta, timezone
from io import BytesIO

from BaboonDefinitions.Generated.baboon_runtime_shared import LEDataOutputStream


class TestTimestampKindByte(unittest.TestCase):
    def _kind(self, d: datetime) -> int:
        out = LEDataOutputStream(BytesIO())
        out.write_datetime(d)
        b = out.stream.getvalue()
        self.assertEqual(17, len(b))
        return b[16]

    def test_kind_byte_is_a_pure_function_of_the_offset(self):
        self.assertEqual(0, self._kind(datetime(2026, 9, 16, 12, tzinfo=timezone(timedelta(hours=5, minutes=30)))))
        self.assertEqual(0, self._kind(datetime(2026, 9, 16, 12, tzinfo=timezone(timedelta(hours=-3)))))
        self.assertEqual(1, self._kind(datetime(2026, 9, 16, 12, tzinfo=timezone.utc)))


if __name__ == "__main__":
    unittest.main()
