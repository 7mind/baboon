import unittest
from unittest import TestCase

from Generated.convtest.testpkg import v1_0_0
from Generated.convtest.testpkg.Adt0 import B1
from Generated.convtest.testpkg import Adt0
from Generated.convtest.testpkg.baboon_runtime import BaboonConversions, RequiredConversions
from Generated.convtest.testpkg.EnumMemberRename import EnumMemberRename
from Generated.convtest.testpkg.v1_0_0.EnumMemberRename import EnumMemberRename as v1_0_0_EnumMemberRename
from Generated.convtest.testpkg.from_1_0_0__EnumMemberRename import Convert__EnumMemberRename__From__1_0_0

class TestConversions(TestCase):
    def test_derived_conversion_adt_autoupgrade(self):
        b1 = v1_0_0.Adt0.B1(f="value")
        conv = BaboonConversions(required=RequiredConversions())

        converted = conv.convert_by_type(None, b1, v1_0_0.Adt0.B1, B1)

        self.assertEqual(b1.f, converted.f)

        converted2 = conv.convert_by_type(None, b1, v1_0_0.Adt0.Adt0, Adt0.Adt0)

        self.assertEqual(converted, converted2)


class TestEnumMemberRenameConversion(TestCase):
    """Regression test for the ``EnumVariant : was[OldName]`` rename
    conversion (see ``test/conv-test/pkg02.baboon`` ``enum
    EnumMemberRename`` and the matching Rust/TypeScript regression tests
    in ``test/conv-test-{rs,ts}/tests/``). Invokes the generated rename
    conversion against both the renamed variant
    (``OldValue`` -> ``NewValue``) and the pass-through variant
    (``KeepValue``) so the fallback arm of the mapping is exercised.
    """

    def test_renamed_variant_maps_to_new_name(self):
        converter = Convert__EnumMemberRename__From__1_0_0(
            v1_0_0_EnumMemberRename, EnumMemberRename
        )
        mapped = converter.do_convert(None, None, v1_0_0_EnumMemberRename.OldValue)
        self.assertEqual(EnumMemberRename.NewValue, mapped)

    def test_non_renamed_variant_passes_through(self):
        converter = Convert__EnumMemberRename__From__1_0_0(
            v1_0_0_EnumMemberRename, EnumMemberRename
        )
        mapped = converter.do_convert(None, None, v1_0_0_EnumMemberRename.KeepValue)
        self.assertEqual(EnumMemberRename.KeepValue, mapped)

    def test_conversion_meta_accessors_resolve_to_string(self):
        converter = Convert__EnumMemberRename__From__1_0_0(
            v1_0_0_EnumMemberRename, EnumMemberRename
        )
        self.assertIsInstance(converter.type_id, str)
        self.assertIsInstance(converter.version_from, str)
        self.assertIsInstance(converter.version_to, str)
