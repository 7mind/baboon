import unittest
from io import BytesIO

from BaboonDefinitions.Generated.baboon_runtime_shared import (
    Lazy,
    LEDataInputStream,
    LEDataOutputStream,
)


class RuntimePrimitivesTest(unittest.TestCase):
    def test_string_length_prefix_boundaries(self):
        for text, prefix in (
            ("", b"\x00"),
            ("a" * 127, b"\x7f"),
            ("a" * 128, b"\x80\x01"),
            ("é" * 64, b"\x80\x01"),
            ("a" * 16383, b"\xff\x7f"),
            ("a" * 16384, b"\x80\x80\x01"),
        ):
            with self.subTest(length=len(text.encode("utf-8"))):
                stream = BytesIO()
                LEDataOutputStream(stream).write_str(text)
                self.assertEqual(prefix + text.encode("utf-8"), stream.getvalue())
                stream.seek(0)
                self.assertEqual(text, LEDataInputStream(stream).read_string())

    def test_lazy_creation_status(self):
        calls = []
        value = object()

        def create():
            calls.append(True)
            return value

        lazy = Lazy(create)
        self.assertFalse(lazy.is_value_created)
        self.assertIs(value, lazy.value)
        self.assertTrue(lazy.is_value_created)
        self.assertIs(value, lazy.value)
        self.assertEqual([True], calls)
