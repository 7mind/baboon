import unittest

from BaboonDefinitions.Generated.baboon_conversions import AbstractBaboonConversions, AbstractConversion
from BaboonDefinitions.Generated.baboon_runtime_shared import BaboonAdtMemberMeta, BaboonGenerated


class Branch(BaboonGenerated, BaboonAdtMemberMeta):
    baboon_domain_identifier = "test"
    baboon_domain_version = "1.0.0"
    baboon_type_identifier = ""
    baboon_same_in_versions = ["1.0.0"]
    baboon_min_reader_versions = {}
    baboon_adt_type_identifier = ""
    baboon_adt_type = object

    def __init__(self, identifier, owner):
        self.baboon_type_identifier = identifier
        self.baboon_adt_type_identifier = owner


class ManualConversion(AbstractConversion):
    version_from = "1.0.0"
    version_to = "2.0.0"
    type_id = "old.branch"

    def __init__(self, result, events):
        super().__init__(Branch, Branch)
        self.result = result
        self.events = events

    def do_convert(self, context, conversions, source):
        self.events.append(("convert", source))
        return self.result


class RenamedConversion(ManualConversion):
    @property
    def _target_type_id(self):
        return "new.branch"


class ConversionIdentifiersTest(unittest.TestCase):
    def test_renamed_branch_uses_target_identifier_after_conversion(self):
        source = Branch("old.branch", "old.root")
        target = Branch("new.branch", "new.root")
        conversion = RenamedConversion(target, [])
        self.assertIs(target, conversion.convert(None, AbstractBaboonConversions(), source))
        self.assertEqual("old.branch", conversion.type_id)

    def test_wrong_source_is_rejected_before_conversion(self):
        events = []
        conversion = RenamedConversion(Branch("new.branch", "new.root"), events)
        with self.assertRaisesRegex(ValueError, "must be old.branch"):
            conversion.convert(None, AbstractBaboonConversions(), Branch("other.branch", "other.root"))
        self.assertEqual([], events)

    def test_wrong_target_is_rejected_after_conversion(self):
        events = []
        source = Branch("old.branch", "old.root")
        conversion = RenamedConversion(Branch("wrong.branch", "new.root"), events)
        with self.assertRaisesRegex(ValueError, "must be new.branch"):
            conversion.convert(None, AbstractBaboonConversions(), source)
        self.assertEqual([("convert", source)], events)

    def test_legacy_source_validator_override_keeps_both_calls(self):
        class TrackingConversion(ManualConversion):
            def validate_baboon_type(self, value):
                self.events.append(("validate", value))
                super().validate_baboon_type(value)

        source = Branch("old.branch", "old.root")
        target = Branch("old.branch", "old.root")
        events = []
        conversion = TrackingConversion(target, events)
        self.assertIs(target, conversion.convert(None, AbstractBaboonConversions(), source))
        self.assertEqual([("validate", source), ("convert", source), ("validate", target)], events)

    def test_renamed_public_validator_accepts_both_endpoints(self):
        conversion = RenamedConversion(Branch("new.branch", "new.root"), [])
        conversion.validate_baboon_type(Branch("old.branch", "old.root"))
        conversion.validate_baboon_type(Branch("new.branch", "new.root"))

    def test_renamed_custom_validator_keeps_both_super_calls(self):
        class TrackingConversion(RenamedConversion):
            def validate_baboon_type(self, value):
                self.events.append(("validate", value))
                super().validate_baboon_type(value)

        source = Branch("old.branch", "old.root")
        target = Branch("new.branch", "new.root")
        events = []
        self.assertIs(target, TrackingConversion(target, events).convert(None, AbstractBaboonConversions(), source))
        self.assertEqual([("validate", source), ("convert", source), ("validate", target)], events)

    def test_renamed_custom_validator_can_reject_result(self):
        for call_super in (False, True):
            with self.subTest(call_super=call_super):
                class RejectingConversion(RenamedConversion):
                    def validate_baboon_type(self, value):
                        if call_super:
                            super().validate_baboon_type(value)
                        if value is self.result:
                            raise ValueError("application result validation")

                source = Branch("old.branch", "old.root")
                target = Branch("new.branch", "new.root")
                with self.assertRaisesRegex(ValueError, "application result validation"):
                    RejectingConversion(target, []).convert(None, AbstractBaboonConversions(), source)

    def test_renamed_conversion_rejects_swapped_endpoints(self):
        source = Branch("old.branch", "old.root")
        target = Branch("new.branch", "new.root")
        events = []
        with self.assertRaisesRegex(ValueError, "must be old.branch"):
            RenamedConversion(target, events).convert(None, AbstractBaboonConversions(), target)
        self.assertEqual([], events)
        with self.assertRaisesRegex(ValueError, "must be new.branch"):
            RenamedConversion(source, events).convert(None, AbstractBaboonConversions(), source)
        self.assertEqual([("convert", source)], events)

    def test_legacy_validator_override_does_not_force_identifier_access(self):
        class CustomValidator(ManualConversion):
            @property
            def type_id(self):
                raise AssertionError("custom validator does not use metadata")

            def validate_baboon_type(self, value):
                self.events.append(("validate", value))

        source = Branch("old.branch", "old.root")
        target = Branch("old.branch", "old.root")
        events = []
        conversion = CustomValidator(target, events)
        self.assertIs(target, conversion.convert(None, AbstractBaboonConversions(), source))
        self.assertEqual([("validate", source), ("convert", source), ("validate", target)], events)

    def test_parent_adt_identifier_remains_accepted(self):
        class ParentConversion(RenamedConversion):
            type_id = "old.root"

            @property
            def _target_type_id(self):
                return "new.root"

        target = Branch("new.branch", "new.root")
        self.assertIs(target, ParentConversion(target, []).convert(
            None, AbstractBaboonConversions(), Branch("old.branch", "old.root")))

    def test_foreign_values_do_not_consult_identifier_hooks(self):
        class ForeignConversion(ManualConversion):
            @property
            def type_id(self):
                raise AssertionError("foreign source must skip metadata")

            @property
            def _target_type_id(self):
                raise AssertionError("foreign result must skip metadata")

        self.assertEqual(42, ForeignConversion(42, []).convert(None, AbstractBaboonConversions(), "input"))
