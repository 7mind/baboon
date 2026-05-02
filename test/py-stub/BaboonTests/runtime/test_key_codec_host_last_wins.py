# PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
#
# Asserts FStr_KeyCodecHost.register(impl) overwrites the previously registered
# impl (last-wins). Python uses a module-global mutable singleton (`global ...`)
# pre-PR-26.2; this test pins that behavior across future refactors.
#
# Generated symbols are produced by mdl :test-gen-regular-adt under
# target/test-regular/py-stub/. This file lives under `BaboonTests/runtime/`
# and is NOT auto-discovered by the action's discovery root; run directly:
#     cd target/test-regular/py-stub && \
#         python3 -m unittest BaboonTests.runtime.test_key_codec_host_last_wins

import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.my.ok.m19.foreign.FStr import (
    FStr_KeyCodec,
    FStr_KeyCodecHost,
)
from BaboonDefinitions.Generated.my.ok.m19.foreign.Holder import Holder, Holder_JsonCodec
from BaboonDefinitions.Generated.my.ok.m19.foreign.ItemKey import ItemKey


class _PrefixCodec:
    def __init__(self, tag: str) -> None:
        self.tag = tag

    def encode_key(self, value: str) -> str:
        return f"{self.tag}:{value}"

    def decode_key(self, s: str) -> str:
        pfx = f"{self.tag}:"
        return s[len(pfx):] if s.startswith(pfx) else s


class _IdentityCodec:
    def encode_key(self, value: str) -> str:
        return value

    def decode_key(self, s: str) -> str:
        return s


class TestKeyCodecHostLastWins(unittest.TestCase):
    def setUp(self) -> None:
        self.ctx = BaboonCodecContext.COMPACT

    def test_register_b_after_register_a_observes_b(self) -> None:
        original = Holder(m={ItemKey(v="k"): "v"})

        FStr_KeyCodecHost.register(_PrefixCodec("A"))
        encoded_a = json.dumps(Holder_JsonCodec().encode(self.ctx, original))
        self.assertIn(
            "A:k", encoded_a,
            f"expected A: prefix in encoded wire form, got {encoded_a}",
        )

        FStr_KeyCodecHost.register(_PrefixCodec("B"))
        encoded_b = json.dumps(Holder_JsonCodec().encode(self.ctx, original))
        self.assertIn(
            "B:k", encoded_b,
            f"PR-26.2 last-wins regression: expected B: prefix after re-register, got {encoded_b}",
        )
        self.assertNotIn(
            "A:k", encoded_b,
            f"PR-26.2 last-wins regression: A: prefix still present after B re-register, got {encoded_b}",
        )

        # Restore identity-encoding default for any subsequent tests.
        FStr_KeyCodecHost.register(_IdentityCodec())


if __name__ == "__main__":
    unittest.main()
