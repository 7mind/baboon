import json
from pathlib import Path
from unittest import TestCase

from Generated.baboon_codecs import BaboonCodecContext
from Generated.convtest.m24foreign.ForeignKeyHolder import ForeignKeyHolder, ForeignKeyHolder_JsonCodec
from Generated.convtest.m24foreign.ItemKey import ItemKey


# ----------------------------------------------------------------------------
# PR-I.2 (M24 Phase 3.2) — Custom-foreign `<Foreign>_KeyCodec` extension hook
# (Python mirror).
#
# Mirrors PR-I.1a (Scala) / PR-I.1b (Java + Kotlin) / PR-I.1c (C#) /
# PR-I.1d (Dart + TS) / PR-I.2 Swift cross-language tests: round-trip the
# Python-emitted m24-foreign-keycodec.json through ForeignKeyHolder_JsonCodec
# and assert byte-identity (PR-I-D02 pattern guidance) of the encoded JSON
# string against the canonical compact wire form
# `{"m":{"alpha":"v1","beta":"v2"}}`.
# ----------------------------------------------------------------------------


class TestM24ForeignKeyCodec(TestCase):
    base_dir = Path("../../target/compat-test").resolve()
    ctx = BaboonCodecContext.default()

    def test_round_trip(self):
        file = self.base_dir / "python-json" / "m24-foreign-keycodec.json"
        self.assertTrue(file.exists(), f"Python m24-foreign-keycodec fixture not found: {file}")
        with open(file, "r", encoding="utf-8") as f:
            json_str = f.read()
        decoded = ForeignKeyHolder_JsonCodec.instance().decode(self.ctx, json_str)
        expected = ForeignKeyHolder(m={
            ItemKey(v="alpha"): "v1",
            ItemKey(v="beta"): "v2",
        })
        self.assertEqual(decoded, expected, "round-trip diverged")

    def test_canonical_wire_form(self):
        sample = ForeignKeyHolder(m={
            ItemKey(v="alpha"): "v1",
            ItemKey(v="beta"): "v2",
        })
        encoded_str = ForeignKeyHolder_JsonCodec.instance().encode(self.ctx, sample)
        # The codec uses `dumps(obj)` which inserts default separators (`, ` and `: `).
        # Re-emit via `json.dumps(..., separators=(',', ':'), sort_keys=True)` so the
        # byte-identity assertion against the canonical compact wire form holds.
        compact = json.dumps(json.loads(encoded_str), separators=(",", ":"), sort_keys=True)
        expected = '{"m":{"alpha":"v1","beta":"v2"}}'
        self.assertEqual(compact, expected, "FStr_KeyCodec wire form diverged")
