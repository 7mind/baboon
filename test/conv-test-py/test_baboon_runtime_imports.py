# PR-25.2 (M25) — D04 reproduction.
#
# Pre-existing import-time `AttributeError` in generated conversion modules: codegen emitted
# `from <pkg>.v<old> import <namespace>` followed by attribute-chain dereferencing into
# implicit-namespace-package directories that did not auto-load their leaf type modules.
# Result: `from Generated.convtest.testpkg.baboon_runtime import ...` failed at module load
# with `AttributeError: module 'Generated.convtest.testpkg.v1_0_0.abs.core' has no attribute
# 'OldAbsAdt'`.
#
# After the fix, `PyBaboonTranslator.renderTree` emits per-leaf direct imports
# (`from <full module path> import <Symbol> as <alias>`) for versioned types, so this
# smoke-import succeeds.

from unittest import TestCase


class TestBaboonRuntimeImports(TestCase):
    def test_baboon_runtime_module_loads(self):
        # Smoke-test: the import itself is the assertion. Pre-fix this raised AttributeError
        # at module load before any test code ran.
        from Generated.convtest.testpkg import baboon_runtime  # noqa: F401

    def test_old_abs_adt_conversion_module_loads(self):
        # Direct import of the namespaced conversion module that triggered the original
        # AttributeError on `v1_0_0.abs.core.OldAbsAdt`.
        from Generated.convtest.testpkg import from_1_0_0_abs_core_OldAbsAdt  # noqa: F401
