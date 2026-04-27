"""
PR 13.2 (M13) cross-language AnyShowcase tests for Python.

Reads `any-showcase.{json,ueba}` fixtures emitted by every other conv-test stub and verifies
each decodes into the same logical sequence of `InnerPayload(label, count)` values. Confirms
wire-format compatibility across the 10-language matrix introduced in M13.

Pre-existing import-time failure in `Generated.convtest.testpkg.baboon_runtime` (an unrelated
malformed conversion-module reference) is sidestepped by importing only the codecs needed.
"""

import io
import json
from pathlib import Path
from unittest import TestCase

from Generated.baboon_any_opaque import AnyOpaqueJson, AnyOpaqueUeba
from Generated.baboon_codecs import BaboonCodecContext
from Generated.baboon_runtime_shared import LEDataInputStream
from Generated.convtest.testpkg.AnyShowcase import AnyShowcase_JsonCodec, AnyShowcase_UEBACodec
from Generated.convtest.testpkg.InnerPayload import InnerPayload, InnerPayload_JsonCodec, InnerPayload_UEBACodec

BASE_DIR = Path("../../target/compat-test").resolve()

EXPECTED = [
    InnerPayload(label="variant-A", count=1),
    InnerPayload(label="variant-B", count=2),
    InnerPayload(label="variant-C", count=3),
    InnerPayload(label="variant-D1", count=4),
    InnerPayload(label="variant-D2", count=5),
    InnerPayload(label="variant-D3", count=6),
    InnerPayload(label="opt-any", count=7),
    InnerPayload(label="lst-any-0", count=8),
]


def _read_json(source):
    fp = BASE_DIR / f"{source}-json" / "any-showcase.json"
    with open(fp, "r", encoding="utf-8") as f:
        return AnyShowcase_JsonCodec.instance().decode(BaboonCodecContext.default(), f.read())


def _read_ueba(source):
    fp = BASE_DIR / f"{source}-ueba" / "any-showcase.ueba"
    with open(fp, "rb") as f:
        return AnyShowcase_UEBACodec.instance().decode(
            BaboonCodecContext.default(), LEDataInputStream(io.BytesIO(f.read()))
        )


def _decode_inner(o):
    if isinstance(o, AnyOpaqueUeba):
        return InnerPayload_UEBACodec.instance().decode(
            BaboonCodecContext.compact(), LEDataInputStream(io.BytesIO(o.bytes))
        )
    if isinstance(o, AnyOpaqueJson):
        return InnerPayload_JsonCodec.instance().decode(
            BaboonCodecContext.compact(), json.dumps(o.json)
        )
    raise RuntimeError(f"unexpected AnyOpaque subclass: {type(o)}")


def _decode_all(v):
    assert v.optAny is not None, "optAny was None"
    assert v.lstAny, "lstAny was empty"
    return [
        _decode_inner(v.vAnyA),
        _decode_inner(v.vAnyB),
        _decode_inner(v.vAnyC),
        _decode_inner(v.vAnyD1),
        _decode_inner(v.vAnyD2),
        _decode_inner(v.vAnyD3),
        _decode_inner(v.optAny),
        _decode_inner(v.lstAny[0]),
    ]


SOURCES = ["python", "scala", "cs", "rust", "java", "kotlin", "typescript", "dart", "swift"]


class TestAnyShowcaseCrossLanguage(TestCase):
    def _assert(self, source, fmt, v):
        decoded = _decode_all(v)
        self.assertEqual(len(decoded), len(EXPECTED))
        for i, (exp, got) in enumerate(zip(EXPECTED, decoded)):
            self.assertEqual(exp, got, f"{source} {fmt} payload {i} mismatch")

    def test_python_json(self):
        self._assert("python", "JSON", _read_json("python"))

    def test_python_ueba(self):
        self._assert("python", "UEBA", _read_ueba("python"))

    def test_scala_json(self):
        self._assert("scala", "JSON", _read_json("scala"))

    def test_scala_ueba(self):
        self._assert("scala", "UEBA", _read_ueba("scala"))

    def test_cs_json(self):
        self._assert("cs", "JSON", _read_json("cs"))

    def test_cs_ueba(self):
        self._assert("cs", "UEBA", _read_ueba("cs"))

    def test_rust_json(self):
        self._assert("rust", "JSON", _read_json("rust"))

    def test_rust_ueba(self):
        self._assert("rust", "UEBA", _read_ueba("rust"))

    def test_java_json(self):
        self._assert("java", "JSON", _read_json("java"))

    def test_java_ueba(self):
        self._assert("java", "UEBA", _read_ueba("java"))

    def test_kotlin_json(self):
        self._assert("kotlin", "JSON", _read_json("kotlin"))

    def test_kotlin_ueba(self):
        self._assert("kotlin", "UEBA", _read_ueba("kotlin"))

    def test_typescript_json(self):
        fp = BASE_DIR / "typescript-json" / "any-showcase.json"
        if not fp.exists():
            self.skipTest("typescript any-showcase JSON not found")
        self._assert("typescript", "JSON", _read_json("typescript"))

    def test_typescript_ueba(self):
        fp = BASE_DIR / "typescript-ueba" / "any-showcase.ueba"
        if not fp.exists():
            self.skipTest("typescript any-showcase UEBA not found")
        self._assert("typescript", "UEBA", _read_ueba("typescript"))

    def test_dart_json(self):
        fp = BASE_DIR / "dart-json" / "any-showcase.json"
        if not fp.exists():
            self.skipTest("dart any-showcase JSON not found")
        self._assert("dart", "JSON", _read_json("dart"))

    def test_dart_ueba(self):
        fp = BASE_DIR / "dart-ueba" / "any-showcase.ueba"
        if not fp.exists():
            self.skipTest("dart any-showcase UEBA not found")
        self._assert("dart", "UEBA", _read_ueba("dart"))

    def test_swift_json(self):
        fp = BASE_DIR / "swift-json" / "any-showcase.json"
        if not fp.exists():
            self.skipTest("swift any-showcase JSON not found")
        self._assert("swift", "JSON", _read_json("swift"))

    def test_swift_ueba(self):
        fp = BASE_DIR / "swift-ueba" / "any-showcase.ueba"
        if not fp.exists():
            self.skipTest("swift any-showcase UEBA not found")
        self._assert("swift", "UEBA", _read_ueba("swift"))

    def test_ueba_byte_identical_python_scala(self):
        py = (BASE_DIR / "python-ueba" / "any-showcase.ueba").read_bytes()
        sc = (BASE_DIR / "scala-ueba" / "any-showcase.ueba").read_bytes()
        self.assertEqual(py, sc, "Python and Scala UEBA bytes diverged")
