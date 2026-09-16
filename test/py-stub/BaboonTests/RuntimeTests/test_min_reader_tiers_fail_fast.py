# Every runtime fails fast when a value's `baboon_min_reader_versions` lacks the tier the envelope
# needs (docs/forward-compat.md, "Envelope integration"). Generated types always carry all four
# tiers; this guards hand-written implementations.
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext, BaboonEnvelopeVersion, ForwardWritePolicy
from BaboonDefinitions.Generated.baboon_codecs_facade import BaboonCodecsFacade
from BaboonDefinitions.Generated.baboon_exceptions import BaboonCodecException
from BaboonDefinitions.Generated.baboon_runtime_shared import BaboonGenerated, BaboonTypeMeta


class _NoTiers(BaboonGenerated):
    def __init__(self, tiers: dict[str, str]):
        self._tiers = tiers

    @property
    def baboon_domain_identifier(self) -> str:
        return "t.d"

    @property
    def baboon_domain_version(self) -> str:
        return "1.0.0"

    @property
    def baboon_type_identifier(self) -> str:
        return "t.d/:#T"

    @property
    def baboon_same_in_versions(self) -> list[str]:
        return ["1.0.0"]

    @property
    def baboon_min_reader_versions(self) -> dict[str, str]:
        return self._tiers


class TestMinReaderTiersFailFast(unittest.TestCase):
    def test_json_bound_missing_raises(self):
        with self.assertRaisesRegex(BaboonCodecException, "json-additive"):
            BaboonTypeMeta.from_instance(_NoTiers({}))
        # with the JSON bound present the meta is built normally
        self.assertEqual("1.0.0", BaboonTypeMeta.from_instance(_NoTiers({"json-additive": "1.0.0"})).domain_version_readable_min)

    def test_prefix_bound_missing_raises_for_v2(self):
        json_only = _NoTiers({"json-additive": "1.0.0"})
        v2 = BaboonCodecContext.custom(False, ForwardWritePolicy.STRICT, BaboonEnvelopeVersion.V2, None)
        with self.assertRaisesRegex(BaboonCodecException, "prefix-compact"):
            BaboonCodecsFacade._bin_type_meta(json_only, v2)
        # the default v1/Strict context needs no prefix tier
        self.assertEqual(1, BaboonCodecsFacade._bin_type_meta(json_only, BaboonCodecContext.Compact).meta_version)


if __name__ == "__main__":
    unittest.main()
