# PR-25.2 (M25) — D03 reproduction.
#
# `BaboonCodecsFacade.json_to_ueba_bytes` accepts a parsed JSON value (dict/list/primitive),
# but the generated Python JSON codec is stringly-typed (its `decode(ctx, wire: str)` routes
# through pydantic's `model_validate_json`). Before the fix, the facade passed the parsed
# `dict` straight to `decode`, triggering `pydantic.ValidationError` ("Input should be a valid
# string"). The fix wraps the value in `json.dumps(...)` before delegation.
#
# This test wires up a minimal facade (Inner DTO from the `my.ok` fixture model registered
# via the generated `BaboonCodecsJson`/`BaboonCodecsUeba` aggregators) and exercises
# `json_to_ueba_bytes` with a `dict` payload. Pre-fix: pydantic raises and the facade returns
# `BaboonLeft`. Post-fix: returns `BaboonRight(<non-empty bytes>)`.
#
# This file lives under `BaboonTests/runtime/` and is NOT auto-discovered by the action's
# `discover -s BaboonTests/GeneratedTests/testpkg/pkg0`. Run directly from the rsync'd
# target copy:
#     cd target/test-regular/py-stub && \
#         python3 -m unittest BaboonTests.runtime.test_facade_cross_format

import unittest

from BaboonDefinitions.Generated.baboon_any_opaque import AnyMeta
from BaboonDefinitions.Generated.baboon_codecs_facade import BaboonCodecsFacade
from BaboonDefinitions.Generated.baboon_runtime_shared import BaboonDomainVersion, BaboonMeta
from BaboonDefinitions.Generated.baboon_service_wiring import BaboonRight

from BaboonDefinitions.Generated.my.ok.baboon_runtime import (
    BaboonCodecsJson,
    BaboonCodecsUeba,
)
from BaboonDefinitions.Generated.my.ok.Inner import Inner


DOMAIN_ID = "my.ok"
VERSION_STR = "1.0.0"
INNER_TYPE = "my.ok/:#Inner"


class _NoConversions:
    """The facade's `verify` checks `versions_conversions`; cross-format helpers do not call
    `verify`, so a trivial stub is enough to satisfy `register`'s signature.
    """


class _MyOkMeta(BaboonMeta):
    @property
    def same_in_versions(self):
        return lambda _typeid: [VERSION_STR]


def _fresh_facade() -> BaboonCodecsFacade:
    facade = BaboonCodecsFacade()
    facade.register(
        BaboonDomainVersion(DOMAIN_ID, VERSION_STR),
        codecs_json=lambda: BaboonCodecsJson(),
        codecs_bin=lambda: BaboonCodecsUeba(),
        conversions=lambda: _NoConversions(),
        meta=lambda: _MyOkMeta(),
    )
    return facade


class FacadeCrossFormatTests(unittest.TestCase):
    def test_json_to_ueba_bytes_accepts_parsed_dict_payload(self):
        # Parsed JSON shape — what a caller naturally has after `json.loads(wire_string)`.
        # Pre-fix: pydantic rejects the dict at `Inner.model_validate_json` and the facade
        # returns BaboonLeft(DecoderFailure). Post-fix: dumps to string first, decodes to
        # `Inner(x=42)`, re-encodes via UEBA, returns BaboonRight(non-empty bytes).
        facade = _fresh_facade()
        meta = AnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE)
        dict_payload = {"x": 42}

        result = facade.json_to_ueba_bytes(meta, dict_payload)

        self.assertIsInstance(result, BaboonRight, msg=f"expected BaboonRight, got {result!r}")
        self.assertIsInstance(result.value, bytes)
        self.assertGreater(len(result.value), 0, "UEBA bytes must be non-empty")

    def test_json_to_ueba_bytes_round_trips_through_inner_ueba(self):
        # Stronger property: the cross-format bytes match what a direct UEBA encode of
        # Inner(x=42) produces. Confirms the fix preserves wire-format compatibility.
        from io import BytesIO

        from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
        from BaboonDefinitions.Generated.baboon_runtime_shared import LEDataOutputStream
        from BaboonDefinitions.Generated.my.ok.Inner import Inner_UEBACodec

        facade = _fresh_facade()
        meta = AnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE)
        result = facade.json_to_ueba_bytes(meta, {"x": 42})
        self.assertIsInstance(result, BaboonRight, msg=f"{result!r}")

        buf = BytesIO()
        Inner_UEBACodec.instance().encode(BaboonCodecContext.compact(), LEDataOutputStream(buf), Inner(x=42))
        self.assertEqual(result.value, buf.getvalue())


if __name__ == "__main__":
    unittest.main()
