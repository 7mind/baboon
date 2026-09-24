import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_codecs_facade import BaboonTypeMeta, BaboonTypeMetaCodec
from BaboonDefinitions.Generated.review.python.facade.domain_facade import DomainReviewPythonFacadeFacade
from BaboonDefinitions.Generated.review.python.facade.baboon_runtime import BaboonConversions, RequiredConversions
from BaboonDefinitions.Generated.baboon_runtime_shared import BaboonDomainVersion
from BaboonDefinitions.Generated.baboon_exceptions import BaboonCodecException
from BaboonDefinitions.Generated.review.python.facade.Stable import Stable
from BaboonDefinitions.Generated.review.python.facade.Evolving import Evolving
from BaboonDefinitions.Generated.review.python.facade.v1_0_0.Stable import Stable as OldStable
from BaboonDefinitions.Generated.review.python.facade.v1_0_0.Evolving import Evolving as OldEvolving
from BaboonDefinitions.Generated.review.python.facade.v1_0_0.baboon_metadata import BaboonMetadata as OldMetadata


class VersionedFacadeTest(unittest.TestCase):
    @staticmethod
    def old_envelope(value):
        meta = BaboonTypeMetaCodec.write_json(BaboonTypeMeta.from_instance(value))
        return json.dumps({**meta, "$c": value.model_dump_json()})

    def test_generated_metadata_lists_compatible_versions(self):
        value = OldStable(value=7)
        self.assertEqual(["1.0.0", "2.0.0"], OldMetadata().same_in_versions(value.baboon_type_identifier))

    def test_compatible_decode_selects_latest_registered_codec(self):
        result = DomainReviewPythonFacadeFacade().decode_from_json(self.old_envelope(OldStable(value=7)))
        self.assertIs(type(result), Stable)
        self.assertEqual(7, result.value)

    def test_latest_decode_runs_generated_conversion(self):
        facade = DomainReviewPythonFacadeFacade()
        facade.register(
            BaboonDomainVersion("review.python.facade", "2.0.0"),
            conversions=lambda: BaboonConversions(RequiredConversions()),
        )
        wire = self.old_envelope(OldEvolving(value=7))
        self.assertIs(type(facade.decode_from_json(wire)), OldEvolving)
        result = facade.decode_from_json_latest(wire, Evolving)
        self.assertEqual(Evolving(value=7, extra=None), result)

    def test_latest_exact_roundtrip_preserves_legacy_text_payload(self):
        facade = DomainReviewPythonFacadeFacade()
        value = Evolving(value=8, extra="current")
        wire = facade.encode_to_json(BaboonCodecContext.default(), value)
        self.assertIsInstance(json.loads(wire)["$c"], str)
        self.assertEqual(value, facade.decode_from_json_latest(wire, Evolving))

    def test_encoder_failure_preserves_original_cause(self):
        cause = ValueError("codec factory failed")

        def fail():
            raise cause

        facade = DomainReviewPythonFacadeFacade()
        facade.register(BaboonDomainVersion("review.python.facade", "2.0.0"), codecs_json=fail)
        with self.assertRaises(BaboonCodecException) as caught:
            facade.encode_to_json(BaboonCodecContext.default(), Evolving(value=8, extra=None))
        self.assertIn("EncoderFailure", str(caught.exception))
        self.assertIs(cause, caught.exception.__cause__)
