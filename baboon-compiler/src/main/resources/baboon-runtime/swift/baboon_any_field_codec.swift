import Foundation

public enum BaboonAnyJsonFieldCodec {
    public static func encodeAnyField(
        _ ctx: BaboonCodecContext,
        _ expectedKind: UInt8,
        _ staticDomain: String?,
        _ staticVersion: String?,
        _ staticTypeid: String?,
        _ value: AnyOpaque
    ) -> Any {
        if value.meta.kind != expectedKind {
            preconditionFailure(
                "any: meta-kind 0x\(String(format: "%02x", value.meta.kind & 0xFF)) " +
                "does not match field-declared 0x\(String(format: "%02x", expectedKind & 0xFF))"
            )
        }
        let anyInner: Any?
        switch value {
        case .json(_, let jsonValue):
            anyInner = jsonValue
        case .ueba(let uebaMeta, let uebaBytes):
            guard let anyFacadeBase = ctx.facade else {
                preconditionFailure(
                    "Cannot encode AnyOpaque.ueba into JSON without a facade reference. " +
                    "Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), " +
                    "or supply AnyOpaque.json directly."
                )
            }
            // Downcast to the concrete facade — the marker base is empty by design (PR 9.1
            // import-cycle break). Construction goes through BaboonCodecContext.withFacade
            // which only accepts BaboonCodecsFacadeBase, but real callers pass BaboonCodecsFacade.
            guard let anyFacade = anyFacadeBase as? BaboonCodecsFacade else {
                preconditionFailure(
                    "BaboonCodecContext.facade is not a BaboonCodecsFacade: " +
                    "\(type(of: anyFacadeBase))"
                )
            }
            let anyConvResult = anyFacade.uebaToJson(
                ctx,
                uebaMeta,
                uebaBytes,
                staticDomain: staticDomain,
                staticVersion: staticVersion,
                staticTypeid: staticTypeid
            )
            switch anyConvResult {
            case .failure(let err):
                preconditionFailure("any: uebaToJson failed: \(err)")
            case .success(let json):
                anyInner = json
            }
        }
        var anyEnvelope = AnyMetaCodec.writeJson(value.meta)
        // Swift `[String: Any]` treats `dict[k] = nil` as "remove key" — use NSNull() so the
        // content envelope key is preserved when the inner JSON is null (PR-08-D06 analog:
        // keep wire-shape consistent with Java/Dart/TS regardless of payload-null state).
        anyEnvelope[AnyMetaCodec.ANY_CONTENT_KEY] = anyInner ?? NSNull()
        return anyEnvelope
    }

    public static func decodeAnyField(_ expectedKind: UInt8, _ wire: Any) throws -> AnyOpaque {
        guard let anyEnvelope = wire as? [String: Any] else {
            throw BaboonCodecException.decoderFailure(
                "any: JSON envelope must be an object",
                nil
            )
        }
        let anyMetaResult = AnyMetaCodec.readJson(anyEnvelope)
        let anyMeta: AnyMeta
        switch anyMetaResult {
        case .failure(let err):
            throw err
        case .success(let m):
            anyMeta = m
        }
        if anyMeta.kind != expectedKind {
            throw BaboonCodecException.decoderFailure(
                "any: wire kind 0x\(String(format: "%02x", anyMeta.kind & 0xFF)) " +
                "does not match field-declared 0x\(String(format: "%02x", expectedKind & 0xFF))",
                nil
            )
        }
        guard anyEnvelope.keys.contains(AnyMetaCodec.ANY_CONTENT_KEY) else {
            throw BaboonCodecException.decoderFailure(
                "any: JSON envelope missing '\(AnyMetaCodec.ANY_CONTENT_KEY)' content key",
                nil
            )
        }
        // `keys.contains(...)` confirmed presence above; the value can still be NSNull() for an
        // explicit JSON null payload — pass through to the AnyOpaque.json variant. Swift
        // dictionary lookup returns `Any?` (None means absent — already excluded), so the
        // force-unwrap is safe inside the contains-guard.
        let anyContent = anyEnvelope[AnyMetaCodec.ANY_CONTENT_KEY]!
        return .json(meta: anyMeta, json: anyContent)
    }
}

public enum BaboonAnyUebaFieldCodec {
    public static func encodeAnyField(
        _ ctx: BaboonCodecContext,
        _ writer: BaboonBinWriter,
        _ expectedKind: UInt8,
        _ staticDomain: String?,
        _ staticVersion: String?,
        _ staticTypeid: String?,
        _ value: AnyOpaque
    ) {
        if value.meta.kind != expectedKind {
            preconditionFailure(
                "any: meta-kind 0x\(String(format: "%02x", value.meta.kind & 0xFF)) " +
                "does not match field-declared 0x\(String(format: "%02x", expectedKind & 0xFF))"
            )
        }
        let anyBlob: Data
        switch value {
        case .ueba(_, let bytes):
            anyBlob = bytes
        case .json(let jsonMeta, let jsonValue):
            guard let anyFacadeBase = ctx.facade else {
                preconditionFailure(
                    "Cannot encode AnyOpaque.json into UEBA without a facade reference. " +
                    "Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), " +
                    "or supply AnyOpaque.ueba directly."
                )
            }
            // Downcast to the concrete facade — the marker base is empty by design (PR 9.1
            // import-cycle break). Construction goes through BaboonCodecContext.withFacade
            // which only accepts BaboonCodecsFacadeBase, but real callers pass BaboonCodecsFacade.
            guard let anyFacade = anyFacadeBase as? BaboonCodecsFacade else {
                preconditionFailure(
                    "BaboonCodecContext.facade is not a BaboonCodecsFacade: " +
                    "\(type(of: anyFacadeBase))"
                )
            }
            let anyConvResult = anyFacade.jsonToUebaBytes(
                ctx,
                jsonMeta,
                jsonValue,
                staticDomain: staticDomain,
                staticVersion: staticVersion,
                staticTypeid: staticTypeid
            )
            switch anyConvResult {
            case .failure(let err):
                preconditionFailure("any: jsonToUebaBytes failed: \(err)")
            case .success(let bytes):
                anyBlob = bytes
            }
        }
        // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
        let anyMetaBuf = BaboonBinWriter()
        AnyMetaCodec.writeBin(value.meta, anyMetaBuf)
        let anyMetaBytes = anyMetaBuf.toData()
        let anyTotalLength = Int32(4 + anyMetaBytes.count + anyBlob.count)
        writer.writeI32(anyTotalLength)
        writer.writeI32(Int32(anyMetaBytes.count))
        writer.writeAll(anyMetaBytes)
        writer.writeAll(anyBlob)
    }

    public static func decodeAnyField(_ wire: BaboonBinReader, _ expectedKind: UInt8) throws -> AnyOpaque {
        let anyTotalLength = wire.readI32()
        if anyTotalLength < 0 {
            throw BaboonCodecException.decoderFailure(
                "any: negative total-length \(anyTotalLength)",
                nil
            )
        }
        let anyMetaLength = wire.readI32()
        if anyMetaLength < 0 {
            throw BaboonCodecException.decoderFailure(
                "any: negative meta-length \(anyMetaLength)",
                nil
            )
        }
        if anyTotalLength < 4 + anyMetaLength {
            throw BaboonCodecException.decoderFailure(
                "any: total-length \(anyTotalLength) smaller than 4 + meta-length \(anyMetaLength)",
                nil
            )
        }
        let (anyMeta, anyBytesRead) = try AnyMetaCodec.readBinWithLength(wire)
        if anyBytesRead > Int(anyMetaLength) {
            throw BaboonCodecException.decoderFailure(
                "any: meta bytes-read \(anyBytesRead) exceeded meta-length window \(anyMetaLength)",
                nil
            )
        }
        if anyBytesRead < Int(anyMetaLength) {
            // Forward-compat: skip future meta-extension bytes within the meta-length window.
            wire.skipBytes(Int(anyMetaLength) - anyBytesRead)
        }
        if anyMeta.kind != expectedKind {
            throw BaboonCodecException.decoderFailure(
                "any: wire kind 0x\(String(format: "%02x", anyMeta.kind & 0xFF)) " +
                "does not match field-declared 0x\(String(format: "%02x", expectedKind & 0xFF))",
                nil
            )
        }
        let anyBlobLen = Int(anyTotalLength) - 4 - Int(anyMetaLength)
        let anyBlob = wire.readNBytes(anyBlobLen)
        return .ueba(meta: anyMeta, bytes: anyBlob)
    }
}
