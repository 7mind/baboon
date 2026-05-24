// Regression test for the `EnumVariant : was[OldName]` rename conversion
// (see test/conv-test/pkg02.baboon `enum EnumMemberRename` and the matching
// Rust/TypeScript regression tests in test/conv-test-{rs,ts}/tests/).
// Invokes the generated rename conversion against:
//   1. a renamed source variant (OldValue -> NewValue);
//   2. a non-renamed variant (KeepValue) — passes through the fallback
//      arm of the generated mapper.

import 'package:conv_test_dt/generated/convtest/testpkg/baboon_conversions.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/enum_member_rename.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/from-1.0.0-enum_member_rename.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/1.0.0/enum_member_rename.dart'
    as v1_0_0_enum_member_rename;
import 'package:test/test.dart';

void main() {
  final convs = BaboonConversions();

  test('renamed variant OldValue maps to NewValue', () {
    final mapped = Convert__EnumMemberRename__From__1_0_0.instance.doConvert(
      null,
      convs,
      v1_0_0_enum_member_rename.EnumMemberRename.OldValue,
    );
    expect(mapped, equals(EnumMemberRename.NewValue));
  });

  test('non-renamed variant KeepValue passes through', () {
    final mapped = Convert__EnumMemberRename__From__1_0_0.instance.doConvert(
      null,
      convs,
      v1_0_0_enum_member_rename.EnumMemberRename.KeepValue,
    );
    expect(mapped, equals(EnumMemberRename.KeepValue));
  });
}
