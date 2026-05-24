// Regression test for the `EnumVariant : was[OldName]` rename conversion.
//
// Two latent codegen defects in `RsConversionTranslator.CopyEnumByName`
// previously emitted a match arm of the shape
//
//     "OldValue" => "crate::…::EnumMemberRename::NewValue".parse().expect("enum parse"),
//
// feeding a fully-qualified-path string into the bare-name `FromStr` impl
// emitted by `RsDefnTranslator.makeEnumRepr`. The `.expect("enum parse")`
// panics at runtime. This test invokes the generated conversion and asserts:
//
//   1. a renamed source variant (`OldValue` → `NewValue`) maps correctly;
//   2. a non-renamed variant (`KeepValue`) round-trips unchanged through
//      the fallback arm.

use baboon_conv_test_rs::convtest::testpkg::from_1_0_0_enum_member_rename::convert__enum_member_rename__from__1_0_0;
use baboon_conv_test_rs::convtest::testpkg::enum_member_rename::EnumMemberRename as NewEnum;
use baboon_conv_test_rs::convtest::testpkg::v1_0_0::enum_member_rename::EnumMemberRename as OldEnum;

#[test]
fn renamed_variant_maps_to_new_name() {
    let converted = convert__enum_member_rename__from__1_0_0(&OldEnum::OldValue);
    assert_eq!(converted, NewEnum::NewValue);
}

#[test]
fn non_renamed_variant_passes_through() {
    let converted = convert__enum_member_rename__from__1_0_0(&OldEnum::KeepValue);
    assert_eq!(converted, NewEnum::KeepValue);
}
