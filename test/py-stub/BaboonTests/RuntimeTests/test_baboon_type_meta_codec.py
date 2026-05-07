# NOTE: This test references generated runtime symbols (BaboonTypeMeta, BaboonTypeMetaCodec)
# which are copied into this stub only by the py-stub codegen path (rsync + codegen into
# target/test-regular/py-stub/).
# Running directly from the source tree may fail with missing symbols; run from the
# codegen'd copy with `python3 -m unittest discover -s BaboonTests/RuntimeTests`.

import unittest

from BaboonDefinitions.Generated.baboon_runtime_shared import (
    BaboonTypeMeta,
    BaboonTypeMetaCodec,
)


def _make_meta(meta_version: int = 1) -> BaboonTypeMeta:
    return BaboonTypeMeta(
        meta_version=meta_version,
        domain_identifier="com.example.dom",
        domain_version="1.2.3",
        domain_version_min_compat="1.2.3",
        type_identifier="com.example.dom/:#MyType",
    )


class BaboonTypeMetaWriteJsonTest(unittest.TestCase):
    def test_write_json_emits_mv_as_int(self):
        meta = _make_meta()
        d = meta.write_json()
        self.assertIn("$mv", d)
        # Must be exactly int, not bool (isinstance(True, int) is True).
        self.assertIs(type(d["$mv"]), int)
        self.assertNotIsInstance(d["$mv"], bool)
        self.assertEqual(d["$mv"], 1)

    def test_write_json_emits_required_fields(self):
        meta = _make_meta()
        d = meta.write_json()
        self.assertEqual(d["$d"], "com.example.dom")
        self.assertEqual(d["$v"], "1.2.3")
        self.assertEqual(d["$t"], "com.example.dom/:#MyType")

    def test_write_json_omits_uv_when_equal_to_v(self):
        meta = _make_meta()
        d = meta.write_json()
        self.assertNotIn("$uv", d)

    def test_write_json_includes_uv_when_differs(self):
        meta = BaboonTypeMeta(
            meta_version=1,
            domain_identifier="com.example.dom",
            domain_version="1.2.3",
            domain_version_min_compat="1.0.0",
            type_identifier="com.example.dom/:#MyType",
        )
        d = meta.write_json()
        self.assertEqual(d["$uv"], "1.0.0")


class BaboonTypeMetaReadMetaJsonTest(unittest.TestCase):
    def _valid_obj(self, **overrides):
        base = {
            "$d": "com.example.dom",
            "$v": "1.2.3",
            "$mv": 1,
            "$t": "com.example.dom/:#MyType",
        }
        base.update(overrides)
        return base

    def test_read_meta_json_accepts_numeric_mv(self):
        result = BaboonTypeMeta.read_meta_json(self._valid_obj())
        self.assertIsNotNone(result)
        self.assertEqual(result.meta_version, 1)
        self.assertEqual(result.domain_identifier, "com.example.dom")

    def test_read_meta_json_accepts_absent_mv(self):
        obj = {
            "$d": "com.example.dom",
            "$v": "1.2.3",
            "$t": "com.example.dom/:#MyType",
        }
        result = BaboonTypeMeta.read_meta_json(obj)
        self.assertIsNotNone(result)


class BaboonTypeMetaReadMetaJsonEdgeCasesTest(unittest.TestCase):
    """Edge-case rejection matrix for the $mv field (D06)."""

    def _valid_obj(self, **overrides):
        base = {
            "$d": "com.example.dom",
            "$v": "1.2.3",
            "$mv": 1,
            "$t": "com.example.dom/:#MyType",
        }
        base.update(overrides)
        return base

    def test_rejects_whitespace_padded_string_mv(self):
        # D03: int("  1  ") succeeds in Python — strict regex must reject.
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": "  1  "}))
        self.assertIsNone(result)

    def test_rejects_bool_mv_true(self):
        # bool is a subclass of int in Python; must still be rejected.
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": True}))
        self.assertIsNone(result)

    def test_rejects_bool_mv_false(self):
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": False}))
        self.assertIsNone(result)

    def test_rejects_float_mv(self):
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": 1.5}))
        self.assertIsNone(result)

    def test_rejects_out_of_range_mv(self):
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": 300}))
        self.assertIsNone(result)

    def test_rejects_negative_mv(self):
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": -1}))
        self.assertIsNone(result)

    def test_rejects_list_mv(self):
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": []}))
        self.assertIsNone(result)

    def test_rejects_dict_mv(self):
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": {}}))
        self.assertIsNone(result)

    def test_rejects_explicit_null_mv(self):
        # PR-7-D11: explicit `$mv: null` is rejected (distinct from absent — `in` vs `.get()`
        # returning None). Decided per-PR-7 to reject everywhere rather than per-backend split.
        result = BaboonTypeMeta.read_meta_json(self._valid_obj(**{"$mv": None}))
        self.assertIsNone(result)


if __name__ == "__main__":
    unittest.main()
