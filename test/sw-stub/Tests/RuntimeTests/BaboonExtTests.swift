// Hand-written tests for the BaboonExt helpers added in MFACADE-PR-5:
//   BaboonMetaProvider.domainVersion
//   BaboonMetaProvider.baboonUnmodifiedSinceVersion
//   BaboonMeta.unmodifiedSinceVersion(_:)
//
// Uses only BaboonRuntime symbols (no generated types) so it builds against
// the bare RuntimeTests target without any generated-code dependency.

import XCTest
import Foundation
@testable import BaboonRuntime

// Minimal stub conforming to BaboonMetaProvider — mirrors the pattern from AnyRoundTripTests.
private struct StubMeta: BaboonMetaProvider {
    var baboonDomainVersion: String
    var baboonDomainIdentifier: String
    var baboonTypeIdentifier: String
    var baboonSameInVersions: [String]
}

// Minimal stub conforming to BaboonMeta (instance-side registry).
private final class StubDomainMeta: BaboonMeta {
    static let baboonDomainVersion: String = "1.0.0"
    static let baboonDomainIdentifier: String = "test.domain"
    static let baboonTypeIdentifier: String = ""
    func sameInVersions(_ typeId: String) -> [String] { return ["1.0.0", "2.0.0"] }
}

final class BaboonExtTests: XCTestCase {

    // --- domainVersion ---

    func testDomainVersion_identifierAndVersion() {
        let stub = StubMeta(
            baboonDomainVersion: "3.1.4",
            baboonDomainIdentifier: "my.domain",
            baboonTypeIdentifier: "MyType",
            baboonSameInVersions: ["2.0.0", "3.1.4"]
        )
        let dv = stub.domainVersion
        XCTAssertEqual(dv.domainIdentifier, "my.domain")
        XCTAssertEqual(dv.domainVersion, "3.1.4")
    }

    func testDomainVersion_descriptionFormat() {
        let stub = StubMeta(
            baboonDomainVersion: "1.0.0",
            baboonDomainIdentifier: "acme.corp",
            baboonTypeIdentifier: "Widget",
            baboonSameInVersions: ["1.0.0"]
        )
        XCTAssertEqual(stub.domainVersion.description, "acme.corp:1.0.0")
    }

    // --- baboonUnmodifiedSinceVersion ---

    func testBaboonUnmodifiedSinceVersion_returnFirstElement() {
        let stub = StubMeta(
            baboonDomainVersion: "5.0.0",
            baboonDomainIdentifier: "dom",
            baboonTypeIdentifier: "T",
            baboonSameInVersions: ["2.0.0", "3.0.0", "5.0.0"]
        )
        XCTAssertEqual(stub.baboonUnmodifiedSinceVersion, "2.0.0")
    }

    func testBaboonUnmodifiedSinceVersion_singleElement() {
        let stub = StubMeta(
            baboonDomainVersion: "1.0.0",
            baboonDomainIdentifier: "dom",
            baboonTypeIdentifier: "T",
            baboonSameInVersions: ["1.0.0"]
        )
        XCTAssertEqual(stub.baboonUnmodifiedSinceVersion, "1.0.0")
    }

    // --- unmodifiedSinceVersion(_:) on BaboonMeta ---

    func testUnmodifiedSinceVersion_returnsFirstFromRegistry() {
        let meta = StubDomainMeta()
        XCTAssertEqual(meta.unmodifiedSinceVersion("any.TypeId"), "1.0.0")
    }

    func testUnmodifiedSinceVersion_doesNotDependOnTypeId() {
        // StubDomainMeta ignores typeId and always returns ["1.0.0", "2.0.0"].
        let meta = StubDomainMeta()
        XCTAssertEqual(meta.unmodifiedSinceVersion("other.Type"), "1.0.0")
    }
}
