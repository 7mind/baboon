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

        converted = conv.convert_with_context(conv, b1, v1_0_0.Adt0.B1, B1)

        self.assertEqual(b1.f, converted.f)

        converted2 = conv.convert_with_context(conv, b1, v1_0_0.Adt0.Adt0, Adt0.Adt0)

        self.assertEqual(converted, converted2)


class TestEnumMemberRenameConversion(TestCase):
    """Regression test for the ``EnumVariant : was[OldName]`` rename conversion
    (see ``test/conv-test/pkg02.baboon`` ``enum EnumMemberRename`` and the
    matching Rust/TypeScript regression tests in
    ``test/conv-test-{rs,ts}/tests/``). Invokes the generated rename
    conversion directly against both the renamed variant
    (``OldValue`` -> ``NewValue``) and the pass-through variant
    (``KeepValue``) to also exercise the fallback arm of the mapping.

    Constructs the generated ``Convert__EnumMemberRename__From__1_0_0``
    directly with the (from_type, to_type) args instead of via the
    ``instance()`` classmethod. The generated ``instance()`` calls
    ``cls()`` with no args, which trips on
    ``AbstractConversion.__init__``'s required ``from_type``/``to_type``
    parameters — that is a separate, pre-existing PyConversionTranslator
    defect (every Python conv-test suite is affected; not introduced by
    this regression test).
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
