import unittest

from BaboonDefinitions.Generated.baboon_conversions import AbstractBaboonConversions
from BaboonDefinitions.Generated.testpkg.pkg0.v1_0_0.T6_D1 import T6_D1 as Old
from BaboonDefinitions.Generated.testpkg.pkg0.v1_0_0.T6_D2 import T6_D2 as OldChild
from BaboonDefinitions.Generated.testpkg.pkg0.v2_0_0.T6_D1 import T6_D1 as New
from BaboonDefinitions.Generated.testpkg.pkg0.v2_0_0.T6_D2 import T6_D2 as NewChild
from BaboonDefinitions.Generated.testpkg.pkg0.v2_0_0.from_1_0_0__T6_D1 import Convert__T6_D1__From__1_0_0
from BaboonDefinitions.Generated.testpkg.pkg0.v2_0_0.from_1_0_0__T6_D2 import Convert__T6_D2__From__1_0_0
from BaboonTests.GeneratedFixtures.testpkg.pkg0.v1_0_0.T6_D1_Fixture import T6_D1_Fixture


class CollectionEvolutionTest(unittest.TestCase):
    def convert(self, source):
        conversions = AbstractBaboonConversions()
        conversions.register(Convert__T6_D2__From__1_0_0(OldChild, NewChild))
        return Convert__T6_D1__From__1_0_0(Old, New).convert(None, conversions, source)

    def test_collection_swaps_preserve_elements(self):
        source = T6_D1_Fixture.random().model_copy(update={
            "fSwapLstSet": [10, 20, 10], "fSwapSetLst": {30, 40},
            "fPrecex2": [50, 60], "fPrecex3": {70, 80},
            "fSwapPrecex1": [90, 100], "fSwapPrecex2": {110, 120},
        })
        result = self.convert(source)
        self.assertEqual({10, 20}, result.fSwapLstSet)
        self.assertEqual({30, 40}, set(result.fSwapSetLst))
        self.assertEqual([50, 60], result.fPrecex2)
        self.assertEqual({70, 80}, result.fPrecex3)
        self.assertEqual({90, 100}, result.fSwapPrecex1)
        self.assertEqual({110, 120}, set(result.fSwapPrecex2))

    def test_absent_optionals_become_empty_sets(self):
        source = T6_D1_Fixture.random().model_copy(update={
            "fSwapOptSet0": None, "fSwapOptSet1": None, "fSwapOptSet2": None,
        })
        result = self.convert(source)
        for value in (result.fSwapOptSet0, result.fSwapOptSet1, result.fSwapOptSet2):
            self.assertIsInstance(value, set)
            self.assertEqual(set(), value)
