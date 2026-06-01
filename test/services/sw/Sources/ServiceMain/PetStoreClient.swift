import Foundation
import BaboonRuntime
import Generated

#if canImport(Glibc)
import Glibc
private let SOCK_STREAM_VALUE = Int32(SOCK_STREAM.rawValue)
#elseif canImport(Darwin)
import Darwin
private let SOCK_STREAM_VALUE = SOCK_STREAM
#endif

private func decodeChunkedHttpBody(_ body: Data) -> Data? {
    var cursor = body.startIndex
    var decoded = Data()

    while true {
        guard let lineEnd = body[cursor...].range(of: Data("\r\n".utf8))?.lowerBound else {
            return nil
        }
        let lenData = body[cursor..<lineEnd]
        guard let lenStr = String(data: lenData, encoding: .utf8)?
            .split(separator: ";", maxSplits: 1, omittingEmptySubsequences: true).first,
              let chunkLen = Int(lenStr.trimmingCharacters(in: .whitespacesAndNewlines), radix: 16)
        else {
            return nil
        }

        cursor = lineEnd + 2
        if chunkLen == 0 {
            return decoded
        }

        guard cursor + chunkLen <= body.endIndex else { return nil }
        decoded.append(body[cursor..<(cursor + chunkLen)])
        cursor += chunkLen

        guard cursor + 2 <= body.endIndex else { return nil }
        cursor += 2
    }
}

private func post(host: String, port: UInt16, path: String, body: Data) -> Data {
    return postWithContentType(host: host, port: port, path: path, contentType: "application/json", body: body)
}

private func postBytes(host: String, port: UInt16, path: String, body: Data) -> Data {
    return postWithContentType(host: host, port: port, path: path, contentType: "application/octet-stream", body: body)
}

private func postWithContentType(host: String, port: UInt16, path: String, contentType: String, body: Data) -> Data {
    let fd = socket(AF_INET, SOCK_STREAM_VALUE, 0)
    assert(fd >= 0, "Failed to create socket")
    defer { close(fd) }

    var addr = sockaddr_in()
    addr.sin_family = sa_family_t(AF_INET)
    addr.sin_port = port.bigEndian
    inet_pton(AF_INET, host, &addr.sin_addr)

    let connectResult = withUnsafePointer(to: &addr) { ptr in
        ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sockPtr in
            connect(fd, sockPtr, socklen_t(MemoryLayout<sockaddr_in>.size))
        }
    }
    assert(connectResult == 0, "Failed to connect to \(host):\(port)")

    var requestHead = "POST \(path) HTTP/1.1\r\n"
    requestHead += "Host: \(host):\(port)\r\n"
    requestHead += "Content-Type: \(contentType)\r\n"
    requestHead += "Content-Length: \(body.count)\r\n"
    requestHead += "Connection: close\r\n"
    requestHead += "\r\n"

    var requestData = Data(requestHead.utf8)
    requestData.append(body)
    requestData.withUnsafeBytes { ptr in
        let base = ptr.baseAddress!
        var sent = 0
        while sent < requestData.count {
            let n = send(fd, base + sent, requestData.count - sent, 0)
            if n <= 0 {
                fatalError("Failed to send request")
            }
            sent += n
        }
    }

    var response = Data()
    var chunk = [UInt8](repeating: 0, count: 65536)
    while true {
        let n = recv(fd, &chunk, chunk.count, 0)
        if n < 0 {
            fatalError("Failed to receive response")
        }
        if n == 0 {
            break
        }
        response.append(contentsOf: chunk[0..<n])
    }

    guard let headerRange = response.range(of: Data("\r\n\r\n".utf8)),
          let header = String(data: response.prefix(upTo: headerRange.lowerBound), encoding: .utf8)
    else {
        fatalError("Malformed HTTP response")
    }

    let statusLine = header.components(separatedBy: "\r\n").first ?? ""
    let statusParts = statusLine.split(separator: " ")
    let statusCode = statusParts.count >= 2 ? Int(statusParts[1]) ?? 0 : 0

    let rawBodyData = Data(response.suffix(from: headerRange.upperBound))
    let lowerHeader = header.lowercased()
    let bodyData: Data
    if lowerHeader.contains("transfer-encoding: chunked") {
        bodyData = decodeChunkedHttpBody(rawBodyData) ?? Data()
    } else {
        bodyData = rawBodyData
    }

    if statusCode != 200 {
        let bodyStr = String(data: bodyData, encoding: .utf8) ?? "<non-utf8>"
        fatalError("HTTP request failed: HTTP \(statusCode): \(bodyStr)")
    }

    return Data(bodyData)
}

enum ClientCodec: String {
    case json
    case ueba
    case both
}

func runClient(host: String, port: UInt16, codec: ClientCodec = .both) {
    // Drive the compiler-generated PetStoreClient. With both codecs active the
    // generated client requires both transports:
    //   - transportJson: encodes/decodes JSON `String` over `application/json`,
    //     wrapping the hand-rolled `post`.
    //   - transportUeba: encodes/decodes binary `Data` over
    //     `application/octet-stream`, wrapping `postBytes`.
    // Both target the same `/service/method` wire paths.
    let client = PetStoreClient(
        transportUeba: { service, method, data in
            postBytes(host: host, port: port, path: "/\(service)/\(method)", body: data)
        },
        transportJson: { service, method, data in
            String(
                decoding: post(host: host, port: port, path: "/\(service)/\(method)", body: Data(data.utf8)),
                as: UTF8.self
            )
        }
    )

    // Captured JSON-pass responses, retained so the UEBA pass can assert
    // byte-for-byte equality across codecs when both passes run.
    var jsonBuddy: petstore.addpet.out?
    var jsonWhiskers: petstore.addpet.out?
    var jsonList: petstore.listpets.out?
    var jsonGetBuddy: petstore.getpet.out?
    var jsonDelete: petstore.deletepet.out?
    var jsonList2: petstore.listpets.out?

    // --- JSON pass (bare-named *Json methods) ---
    if codec == .json || codec == .both {
        _ = post(host: host, port: port, path: "/reset", body: Data())

        let buddy = try! client.addPetJson(
            arg: petstore.addpet.`in`(name: "Buddy", status: .Available, tag: "dog")
        )
        let buddyId = buddy.pet.id
        precondition(buddy.pet.name == "Buddy", "expected name Buddy, got \(buddy.pet.name)")

        let whiskers = try! client.addPetJson(
            arg: petstore.addpet.`in`(name: "Whiskers", status: .Pending, tag: "cat")
        )
        let whiskersId = whiskers.pet.id
        precondition(whiskers.pet.name == "Whiskers", "expected name Whiskers, got \(whiskers.pet.name)")

        let list = try! client.listPetsJson(arg: petstore.listpets.`in`())
        precondition(list.pets.count == 2, "expected 2 pets, got \(list.pets.count)")

        let getBuddy = try! client.getPetJson(arg: petstore.getpet.`in`(id: buddyId))
        precondition(getBuddy.pet.name == "Buddy", "expected Buddy, got \(getBuddy.pet.name)")
        precondition(getBuddy.pet.status == .Available, "expected Available, got \(getBuddy.pet.status)")
        precondition(getBuddy.pet.tag == "dog", "expected tag dog, got \(String(describing: getBuddy.pet.tag))")

        let delete = try! client.deletePetJson(arg: petstore.deletepet.`in`(id: whiskersId))
        precondition(delete.deleted == true, "expected deleted=true, got \(delete.deleted)")

        let list2 = try! client.listPetsJson(arg: petstore.listpets.`in`())
        precondition(list2.pets.count == 1, "expected 1 pet, got \(list2.pets.count)")
        precondition(list2.pets[0].name == "Buddy", "expected remaining pet Buddy, got \(list2.pets[0].name)")

        jsonBuddy = buddy
        jsonWhiskers = whiskers
        jsonList = list
        jsonGetBuddy = getBuddy
        jsonDelete = delete
        jsonList2 = list2
    }

    // --- UEBA pass (bare-named methods over the binary transport) ---
    // Replays the same scenario through the binary codec. When the JSON pass
    // also ran, asserts that every response is identical to it. Generated `out`
    // types are Equatable, so `==` is an exact whole-value comparison.
    if codec == .ueba || codec == .both {
        _ = post(host: host, port: port, path: "/reset", body: Data())

        let uebaBuddy = try! client.addPet(
            arg: petstore.addpet.`in`(name: "Buddy", status: .Available, tag: "dog")
        )
        let uebaBuddyId = uebaBuddy.pet.id
        precondition(uebaBuddy.pet.name == "Buddy", "expected name Buddy, got \(uebaBuddy.pet.name)")
        if let j = jsonBuddy {
            precondition(uebaBuddy == j, "UEBA addPet(Buddy) differs from JSON: \(uebaBuddy) vs \(j)")
        }

        let uebaWhiskers = try! client.addPet(
            arg: petstore.addpet.`in`(name: "Whiskers", status: .Pending, tag: "cat")
        )
        let uebaWhiskersId = uebaWhiskers.pet.id
        precondition(uebaWhiskers.pet.name == "Whiskers", "expected name Whiskers, got \(uebaWhiskers.pet.name)")
        if let j = jsonWhiskers {
            precondition(uebaWhiskers == j, "UEBA addPet(Whiskers) differs from JSON: \(uebaWhiskers) vs \(j)")
        }

        let uebaList = try! client.listPets(arg: petstore.listpets.`in`())
        precondition(uebaList.pets.count == 2, "expected 2 pets, got \(uebaList.pets.count)")
        if let j = jsonList {
            precondition(uebaList == j, "UEBA listPets differs from JSON: \(uebaList) vs \(j)")
        }

        let uebaGetBuddy = try! client.getPet(arg: petstore.getpet.`in`(id: uebaBuddyId))
        precondition(uebaGetBuddy.pet.name == "Buddy", "expected Buddy, got \(uebaGetBuddy.pet.name)")
        precondition(uebaGetBuddy.pet.status == .Available, "expected Available, got \(uebaGetBuddy.pet.status)")
        precondition(uebaGetBuddy.pet.tag == "dog", "expected tag dog, got \(String(describing: uebaGetBuddy.pet.tag))")
        if let j = jsonGetBuddy {
            precondition(uebaGetBuddy == j, "UEBA getPet(Buddy) differs from JSON: \(uebaGetBuddy) vs \(j)")
        }

        let uebaDelete = try! client.deletePet(arg: petstore.deletepet.`in`(id: uebaWhiskersId))
        precondition(uebaDelete.deleted == true, "expected deleted=true, got \(uebaDelete.deleted)")
        if let j = jsonDelete {
            precondition(uebaDelete == j, "UEBA deletePet differs from JSON: \(uebaDelete) vs \(j)")
        }

        let uebaList2 = try! client.listPets(arg: petstore.listpets.`in`())
        precondition(uebaList2.pets.count == 1, "expected 1 pet, got \(uebaList2.pets.count)")
        precondition(uebaList2.pets[0].name == "Buddy", "expected remaining pet Buddy, got \(uebaList2.pets[0].name)")
        if let j = jsonList2 {
            precondition(uebaList2 == j, "UEBA listPets (post-delete) differs from JSON: \(uebaList2) vs \(j)")
        }
    }

    print("OK")
}
