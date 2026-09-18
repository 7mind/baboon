# Cross-language golden bytes for the top-level binary envelope (docs/spec/codec-envelope.md §2.1, §2.1.2, §2.1.3; docs/forward-compat.md, "Worked examples"). The same values must produce these exact bytes in every backend; the Scala and TypeScript stubs assert the same sequences structurally.
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext, BaboonEnvelopeVersion, ForwardWritePolicy
from BaboonDefinitions.Generated.fwde2e.chain.ChainAppend import ChainAppend
from BaboonDefinitions.Generated.fwde2e.chain.domain_facade import DomainFwde2eChainFacade
from BaboonDefinitions.Generated.fwde2e.fwd.FwdAppendVar import FwdAppendVar
from BaboonDefinitions.Generated.fwde2e.fwd.FwdEnumGrows import FwdEnumGrows
from BaboonDefinitions.Generated.fwde2e.fwd.FwdEnumHost import FwdEnumHost
from BaboonDefinitions.Generated.fwde2e.fwd.FwdStable import FwdStable
from BaboonDefinitions.Generated.fwde2e.fwd.domain_facade import DomainFwde2eFwdFacade


def hx(b: bytes) -> str:
    return " ".join(f"{x:02X}" for x in b)


V1_TOLERANT = BaboonCodecContext.custom(False, ForwardWritePolicy.TOLERANT, BaboonEnvelopeVersion.V1, None)
V2_COMPACT = BaboonCodecContext.custom(False, ForwardWritePolicy.STRICT, BaboonEnvelopeVersion.V2, None)
V2_INDEXED = BaboonCodecContext.custom(True, ForwardWritePolicy.STRICT, BaboonEnvelopeVersion.V2, None)


class TestBinEnvelopeGolden(unittest.TestCase):
    def test_default_contexts_write_v1_strict(self):
        self.assertEqual(BaboonEnvelopeVersion.V1, BaboonCodecContext.Compact.envelope_version)
        self.assertEqual(ForwardWritePolicy.STRICT, BaboonCodecContext.Compact.forward_write_policy)
        self.assertEqual(BaboonEnvelopeVersion.V1, BaboonCodecContext.Indexed.envelope_version)

    def test_multi_version_domain_facade_is_constructible(self):
        # regression: `_register_version` sorted by `v.version.version` and raised AttributeError
        # for every domain with more than one version, so no multi-version facade could be built
        DomainFwde2eFwdFacade()
        DomainFwde2eChainFacade()

    def test_envelopes_match_the_cross_language_golden_bytes(self):
        fwd = DomainFwde2eFwdFacade()
        chain = DomainFwde2eChainFacade()
        app = FwdAppendVar(a=42, b="hi", t="t")
        # FwdAppendVar, v1 Strict (default) compact: identical bound elided
        self.assertEqual("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(fwd.encode_to_bin(BaboonCodecContext.Compact, app)))
        # FwdAppendVar, v1 Tolerant compact: prefix-compact bound 1.0.0 in the single slot
        self.assertEqual("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(fwd.encode_to_bin(V1_TOLERANT, app)))
        # FwdAppendVar, v2 compact: flags 0b10, readableMin 1.0.0
        self.assertEqual("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(fwd.encode_to_bin(V2_COMPACT, app)))
        # FwdAppendVar, v2 indexed: flags 0 (prefix-any-mode bound is 2.0.0)
        self.assertEqual("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74", hx(fwd.encode_to_bin(V2_INDEXED, app)))
        # FwdStable, v1 Strict compact: byte-identical since 1.0.0 -> hasMinCompat 1
        self.assertEqual("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73", hx(fwd.encode_to_bin(BaboonCodecContext.Compact, FwdStable(s="s"))))
        # FwdStable, v2 compact: flags 0b01, minCompat 1.0.0, readableMin elided
        self.assertEqual("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73", hx(fwd.encode_to_bin(V2_COMPACT, FwdStable(s="s"))))
        # FwdEnumHost, v2 compact: flags 0, no bound
        self.assertEqual("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02", hx(fwd.encode_to_bin(V2_COMPACT, FwdEnumHost(e=FwdEnumGrows.C))))
        # ChainAppend 3.0.0, v2 compact: flags 0b10, readableMin 1.0.0
        self.assertEqual("02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63", hx(chain.encode_to_bin(V2_COMPACT, ChainAppend(a=1, b="b", c="c"))))

    def test_v2_envelope_round_trips_through_its_own_facade(self):
        fwd = DomainFwde2eFwdFacade()
        app = FwdAppendVar(a=42, b="hi", t="t")
        self.assertEqual(app, fwd.decode_from_bin_bytes(fwd.encode_to_bin(V2_COMPACT, app)))


if __name__ == "__main__":
    unittest.main()
