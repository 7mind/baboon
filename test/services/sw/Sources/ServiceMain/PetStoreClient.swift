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

private let ctx = BaboonCodecContext.defaultCtx

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
    requestHead += "Content-Type: application/json\r\n"
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

private func encodeToJson(_ value: Any) -> Data {
    return try! JSONSerialization.data(withJSONObject: value, options: [.sortedKeys])
}

private func decodeFromJson(_ data: Data) -> Any {
    return try! JSONSerialization.jsonObject(with: data, options: [])
}

func runClient(host: String, port: UInt16) {
    // Reset
    _ = post(host: host, port: port, path: "/reset", body: Data())

    // Add Buddy
    let addBuddyIn = petstore.addpet.`in`(name: "Buddy", status: .Available, tag: "dog")
    let addBuddyBody = encodeToJson(petstore.addpet.in_JsonCodec.instance.encode(ctx, addBuddyIn))
    let addBuddyResp = post(host: host, port: port, path: "/PetStore/addPet", body: addBuddyBody)
    let addBuddyOut = try! petstore.addpet.out_JsonCodec.instance.decode(ctx, decodeFromJson(addBuddyResp))
    let buddyId = addBuddyOut.pet.id
    assert(addBuddyOut.pet.name == "Buddy", "expected name Buddy, got \(addBuddyOut.pet.name)")

    // Add Whiskers
    let addWhiskersIn = petstore.addpet.`in`(name: "Whiskers", status: .Pending, tag: "cat")
    let addWhiskersBody = encodeToJson(petstore.addpet.in_JsonCodec.instance.encode(ctx, addWhiskersIn))
    let addWhiskersResp = post(host: host, port: port, path: "/PetStore/addPet", body: addWhiskersBody)
    let addWhiskersOut = try! petstore.addpet.out_JsonCodec.instance.decode(ctx, decodeFromJson(addWhiskersResp))
    let whiskersId = addWhiskersOut.pet.id
    assert(addWhiskersOut.pet.name == "Whiskers", "expected name Whiskers, got \(addWhiskersOut.pet.name)")

    // List pets (expect 2)
    let listIn = petstore.listpets.`in`()
    let listBody = encodeToJson(petstore.listpets.in_JsonCodec.instance.encode(ctx, listIn))
    let listResp = post(host: host, port: port, path: "/PetStore/listPets", body: listBody)
    let listOut = try! petstore.listpets.out_JsonCodec.instance.decode(ctx, decodeFromJson(listResp))
    assert(listOut.pets.count == 2, "expected 2 pets, got \(listOut.pets.count)")

    // Get Buddy
    let getBuddyIn = petstore.getpet.`in`(id: buddyId)
    let getBuddyBody = encodeToJson(petstore.getpet.in_JsonCodec.instance.encode(ctx, getBuddyIn))
    let getBuddyResp = post(host: host, port: port, path: "/PetStore/getPet", body: getBuddyBody)
    let getBuddyOut = try! petstore.getpet.out_JsonCodec.instance.decode(ctx, decodeFromJson(getBuddyResp))
    assert(getBuddyOut.pet.name == "Buddy", "expected Buddy, got \(getBuddyOut.pet.name)")
    assert(getBuddyOut.pet.status == .Available, "expected Available, got \(getBuddyOut.pet.status)")
    assert(getBuddyOut.pet.tag == "dog", "expected tag dog, got \(String(describing: getBuddyOut.pet.tag))")

    // Delete Whiskers
    let deleteIn = petstore.deletepet.`in`(id: whiskersId)
    let deleteBody = encodeToJson(petstore.deletepet.in_JsonCodec.instance.encode(ctx, deleteIn))
    let deleteResp = post(host: host, port: port, path: "/PetStore/deletePet", body: deleteBody)
    let deleteOut = try! petstore.deletepet.out_JsonCodec.instance.decode(ctx, decodeFromJson(deleteResp))
    assert(deleteOut.deleted == true, "expected deleted=true, got \(deleteOut.deleted)")

    // List pets again (expect 1)
    let list2Resp = post(host: host, port: port, path: "/PetStore/listPets", body: listBody)
    let list2Out = try! petstore.listpets.out_JsonCodec.instance.decode(ctx, decodeFromJson(list2Resp))
    assert(list2Out.pets.count == 1, "expected 1 pet, got \(list2Out.pets.count)")
    assert(list2Out.pets[0].name == "Buddy", "expected remaining pet Buddy, got \(list2Out.pets[0].name)")

    print("OK")
}
