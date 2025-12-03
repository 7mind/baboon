from unittest import TestCase

from Generated.convtest.testpkg import v1_0_0
from Generated.convtest.testpkg.Adt0 import B1
from Generated.convtest.testpkg import Adt0
from Generated.convtest.testpkg.baboon_runtime import BaboonConversions, RequiredConversions

class TestConversions(TestCase):
    def test_derived_conversion_adt_autoupgrade(self):
        b1 = v1_0_0.Adt0.B1(f="value")
        conv = BaboonConversions(required=RequiredConversions())

        converted = conv.convert_with_context(conv, b1, v1_0_0.Adt0.B1, B1)

        self.assertEqual(b1.f, converted.f)

        converted2 = conv.convert_with_context(conv, b1, v1_0_0.Adt0.Adt0, Adt0.Adt0)

        self.assertEqual(converted, converted2)
