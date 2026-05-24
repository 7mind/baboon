// Regression test for the `EnumVariant : was[OldName]` rename conversion
// (see test/conv-test/pkg02.baboon `enum EnumMemberRename` and the matching
// Rust/TypeScript regression tests in test/conv-test-{rs,ts}/tests/).
// Invokes the generated rename conversion against:
//   1. a renamed source variant (OldValue -> NewValue);
//   2. a non-renamed variant (KeepValue) — passes through the fallback
//      arm of the generated mapper.

import XCTest
import Foundation
import BaboonRuntime
@testable import ConvtestTestpkg
import ConvtestTestpkg_v1_0_0

final class EnumMemberRenameTests: XCTestCase {

    func testRenamedVariantMapsToNewName() {
        let convs = BaboonConversions_Convtest_Testpkg()
        let mapped = Convert__EnumMemberRename__From__1_0_0.instance.doConvert(
            nil, convs, ConvtestTestpkg_v1_0_0.EnumMemberRename.OldValue
        )
        XCTAssertEqual(mapped, ConvtestTestpkg.EnumMemberRename.NewValue)
    }

    func testNonRenamedVariantPassesThrough() {
        let convs = BaboonConversions_Convtest_Testpkg()
        let mapped = Convert__EnumMemberRename__From__1_0_0.instance.doConvert(
            nil, convs, ConvtestTestpkg_v1_0_0.EnumMemberRename.KeepValue
        )
        XCTAssertEqual(mapped, ConvtestTestpkg.EnumMemberRename.KeepValue)
    }
}
