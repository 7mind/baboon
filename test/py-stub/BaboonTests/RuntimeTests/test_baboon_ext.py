# Hand-written tests for the three BaboonExt module-level helpers added in MFACADE-PR-5:
#   domain_version, baboon_unmodified_since_version, unmodified_since_version.
#
# NOTE: References generated symbols from my.ok (Inner) which are copied into this stub
# only by the py-stub codegen path (rsync + codegen into target/test-regular/py-stub/).
# Run from the codegen'd copy:
#   cd target/test-regular/py-stub && \
#     python3 -m unittest BaboonTests.RuntimeTests.test_baboon_ext

import unittest

from BaboonDefinitions.Generated.baboon_runtime_shared import (
    BaboonDomainVersion,
    BaboonMeta,
    baboon_unmodified_since_version,
    domain_version,
    unmodified_since_version,
)
from BaboonDefinitions.Generated.my.ok.Inner import Inner


DOMAIN_ID = "my.ok"
VERSION_STR = "1.0.0"


class _TestMeta(BaboonMeta):
    """Minimal BaboonMeta stub whose same_in_versions returns a callable per
    the concrete-implementation convention (matching generated BaboonMetadata)."""

    @property
    def same_in_versions(self):
        return lambda _typeid: [VERSION_STR]


class DomainVersionTest(unittest.TestCase):
    def test_domain_version_returns_correct_baboon_domain_version(self):
        inner = Inner(x=7)
        dv = domain_version(inner)
        self.assertIsInstance(dv, BaboonDomainVersion)
        self.assertEqual(dv, BaboonDomainVersion(inner.baboon_domain_identifier, inner.baboon_domain_version))


class BaboonUnmodifiedSinceVersionTest(unittest.TestCase):
    def test_baboon_unmodified_since_version_returns_first_element(self):
        inner = Inner(x=7)
        result = baboon_unmodified_since_version(inner)
        self.assertEqual(result, inner.baboon_same_in_versions[0])


class UnmodifiedSinceVersionTest(unittest.TestCase):
    def test_unmodified_since_version_returns_first_element_for_type_id(self):
        meta = _TestMeta()
        inner = Inner(x=7)
        result = unmodified_since_version(meta, inner.baboon_type_identifier)
        self.assertEqual(result, VERSION_STR)


if __name__ == "__main__":
    unittest.main()
