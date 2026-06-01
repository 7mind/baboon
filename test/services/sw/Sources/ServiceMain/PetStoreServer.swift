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

private let CONTENT_TYPE_JSON = "application/json"
private let CONTENT_TYPE_OCTET_STREAM = "application/octet-stream"

private struct HttpRequest {
    let method: String
    let path: String
    let contentType: String
    let body: Data
}

private func findHeaderEndOffset(in data: Data) -> Int? {
    let marker = Data("\r\n\r\n".utf8)
    guard let range = data.range(of: marker) else { return nil }
    return range.lowerBound
}

private func parseContentHeaders(_ headerLines: [String]) -> (contentLength: Int, chunked: Bool) {
    var contentLength = 0
    var chunked = false
    for line in headerLines {
        let lower = line.lowercased()
        if lower.hasPrefix("content-length:") {
            let valStr = line.dropFirst("content-length:".count).trimmingCharacters(in: .whitespaces)
            contentLength = Int(valStr) ?? 0
        } else if lower.hasPrefix("transfer-encoding:") && lower.contains("chunked") {
            chunked = true
        }
    }
    return (contentLength, chunked)
}

private func decodeChunkedBody(_ body: Data) -> Data? {
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

private func parseHttpRequest(from data: Data) -> HttpRequest? {
    guard let headerEnd = findHeaderEndOffset(in: data) else { return nil }
    guard let headerStr = String(data: data.prefix(headerEnd), encoding: .utf8) else { return nil }
    let lines = headerStr.components(separatedBy: "\r\n")
    guard let requestLine = lines.first else { return nil }

    let parts = requestLine.split(separator: " ", maxSplits: 2)
    guard parts.count >= 2 else { return nil }

    let method = String(parts[0])
    let path = String(parts[1])

    var contentType = ""
    for line in lines.dropFirst() {
        if line.lowercased().hasPrefix("content-type:") {
            contentType = line.dropFirst("content-type:".count)
                .trimmingCharacters(in: .whitespaces)
                .lowercased()
        }
    }

    let parsedHeaders = parseContentHeaders(Array(lines.dropFirst()))
    let payload = data.dropFirst(headerEnd + 4)
    let body: Data

    if parsedHeaders.chunked {
        guard let decoded = decodeChunkedBody(Data(payload)) else { return nil }
        body = decoded
    } else {
        body = Data(payload.prefix(parsedHeaders.contentLength))
    }

    return HttpRequest(method: method, path: path, contentType: contentType, body: body)
}

private func sendResponse(fd: Int32, statusCode: Int, statusText: String, contentType: String, body: Data) {
    var response = "HTTP/1.1 \(statusCode) \(statusText)\r\n"
    response += "Content-Type: \(contentType)\r\n"
    response += "Content-Length: \(body.count)\r\n"
    response += "Connection: close\r\n"
    response += "\r\n"
    var headerData = Data(response.utf8)
    headerData.append(body)
    headerData.withUnsafeBytes { ptr in
        let base = ptr.baseAddress!
        var sent = 0
        while sent < headerData.count {
            let n = send(fd, base + sent, headerData.count - sent, 0)
            if n <= 0 { break }
            sent += n
        }
    }
}

private func sendText(fd: Int32, statusCode: Int, statusText: String, text: String) {
    sendResponse(fd: fd, statusCode: statusCode, statusText: statusText, contentType: "text/plain", body: Data(text.utf8))
}

private func sendJson(fd: Int32, body: Data) {
    sendResponse(fd: fd, statusCode: 200, statusText: "OK", contentType: CONTENT_TYPE_JSON, body: body)
}

private func sendBinary(fd: Int32, body: Data) {
    sendResponse(fd: fd, statusCode: 200, statusText: "OK", contentType: CONTENT_TYPE_OCTET_STREAM, body: body)
}

private func handleConnection(fd: Int32, impl: PetStoreImpl, serverFd: Int32, keepRunning: inout Bool) {
    defer { close(fd) }

    var buffer = Data()
    var temp = [UInt8](repeating: 0, count: 65536)
    var headerComplete = false
    var contentLength = 0
    var headerEndOffset = 0
    var transferChunked = false

    while true {
        let n = recv(fd, &temp, temp.count, 0)
        if n <= 0 { return }
        buffer.append(contentsOf: temp[0..<n])

        if !headerComplete {
            if let end = findHeaderEndOffset(in: buffer),
               let headerStr = String(data: buffer.prefix(end), encoding: .utf8) {
                headerComplete = true
                headerEndOffset = end + 4
                let parsed = parseContentHeaders(headerStr.components(separatedBy: "\r\n"))
                contentLength = parsed.contentLength
                transferChunked = parsed.chunked
            }
        }

        if headerComplete {
            if transferChunked {
                if buffer.range(of: Data("\r\n0\r\n\r\n".utf8), options: [], in: headerEndOffset..<buffer.count) != nil {
                    break
                }
            } else if buffer.count >= headerEndOffset + contentLength {
                break
            }
        }
    }

    guard let request = parseHttpRequest(from: buffer) else {
        sendText(fd: fd, statusCode: 400, statusText: "Bad Request", text: "Bad Request")
        return
    }

    if request.method == "GET" && request.path == "/health" {
        sendText(fd: fd, statusCode: 200, statusText: "OK", text: "ok")
        return
    }

    if request.method == "POST" && request.path == "/reset" {
        impl.reset()
        sendText(fd: fd, statusCode: 200, statusText: "OK", text: "ok")
        return
    }

    if request.method == "POST" && request.path == "/shutdown" {
        sendText(fd: fd, statusCode: 200, statusText: "OK", text: "ok")
        keepRunning = false
        shutdown(serverFd, Int32(SHUT_RDWR))
        close(serverFd)
        return
    }

    if request.method == "POST" {
        let pathParts = request.path.split(separator: "/").map(String.init)
        if pathParts.count == 2 {
            let service = pathParts[0]
            let method = pathParts[1]
            let methodId = BaboonMethodId(serviceId: service, methodName: method)
            do {
                if request.contentType.hasPrefix(CONTENT_TYPE_OCTET_STREAM) {
                    // UEBA: decode/dispatch/encode through the generated binary wiring.
                    let respData = try PetStoreWiring.invokeUeba(methodId, request.body, impl, ctx)
                    sendBinary(fd: fd, body: respData)
                    return
                } else {
                    // JSON (default): route the raw request body string through the
                    // generated JSON wiring, which expects an encoded JSON String.
                    let bodyStr = String(data: request.body, encoding: .utf8) ?? "{}"
                    let respStr = try PetStoreWiring.invokeJson(methodId, bodyStr, impl, ctx)
                    sendJson(fd: fd, body: Data(respStr.utf8))
                    return
                }
            } catch let wiring as BaboonWiringException {
                // Unknown method -> 404 so the client surfaces a clear status.
                if case .noMatchingMethod = wiring.error {
                    sendText(fd: fd, statusCode: 404, statusText: "Not Found", text: "Not Found")
                    return
                }
                sendText(fd: fd, statusCode: 500, statusText: "Internal Server Error", text: String(describing: wiring.error))
                return
            } catch {
                sendText(fd: fd, statusCode: 500, statusText: "Internal Server Error", text: String(describing: error))
                return
            }
        }
    }

    sendText(fd: fd, statusCode: 404, statusText: "Not Found", text: "Not Found")
}

func startServer(host: String, port: UInt16) {
    let impl = PetStoreImpl()

    let serverFd = socket(AF_INET, SOCK_STREAM_VALUE, 0)
    assert(serverFd >= 0, "Failed to create socket")

    var optVal: Int32 = 1
    setsockopt(serverFd, SOL_SOCKET, SO_REUSEADDR, &optVal, socklen_t(MemoryLayout<Int32>.size))

    var addr = sockaddr_in()
    addr.sin_family = sa_family_t(AF_INET)
    addr.sin_port = port.bigEndian
    inet_pton(AF_INET, host, &addr.sin_addr)

    let bindResult = withUnsafePointer(to: &addr) { ptr in
        ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sockPtr in
            bind(serverFd, sockPtr, socklen_t(MemoryLayout<sockaddr_in>.size))
        }
    }
    assert(bindResult == 0, "Failed to bind to \(host):\(port)")

    let listenResult = listen(serverFd, 128)
    assert(listenResult == 0, "Failed to listen")

    signal(SIGPIPE, SIG_IGN)

    print("Listening on \(host):\(port)")
    fflush(stdout)

    var keepRunning = true
    while keepRunning {
        var clientAddr = sockaddr_in()
        var clientLen = socklen_t(MemoryLayout<sockaddr_in>.size)
        let clientFd = withUnsafeMutablePointer(to: &clientAddr) { ptr in
            ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sockPtr in
                accept(serverFd, sockPtr, &clientLen)
            }
        }
        if clientFd < 0 { continue }
        handleConnection(fd: clientFd, impl: impl, serverFd: serverFd, keepRunning: &keepRunning)
    }
}
