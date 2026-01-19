

import json
from io import BytesIO
from typing import Dict, List, Optional, Callable, TypeVar, Type, Tuple, Any

from BaboonDefinitions.Generated.baboon_codecs import AbstractBaboonJsonCodecs, AbstractBaboonUebaCodecs,
    LEDataOutputStream, LEDataInputStream, BaboonCodecContext, BaboonBinCodec, BaboonJsonCodec, BaboonCodecData
from BaboonDefinitions.Generated.baboon_conversions import AbstractBaboonConversions
from BaboonDefinitions.Generated.baboon_exceptions import BaboonCodecException
from BaboonDefinitions.Generated.baboon_runtime_shared import BaboonDomainVersion, Lazy,
    BaboonMeta, Version, BaboonGenerated, BaboonTypeMeta, BaboonGeneratedLatest, BaboonTypeMetaCodec,
    BaboonAdtMemberMeta

TI = TypeVar("TI", bound=BaboonGenerated)
TO = TypeVar("TO", bound=BaboonGeneratedLatest)


class BaboonCodecsFacade:
    CONTENT_JSON_KEY = "$c"

    def __init__(self):
        self.versions_codecs_json: Dict[BaboonDomainVersion, Lazy[AbstractBaboonJsonCodecs]] = {}
        self.versions_codecs_bin: Dict[BaboonDomainVersion, Lazy[AbstractBaboonUebaCodecs]] = {}
        self.versions_conversions: Dict[BaboonDomainVersion, Lazy[AbstractBaboonConversions]] = {}
        self.versions_meta: Dict[BaboonDomainVersion, Lazy[BaboonMeta]] = {}
        self.domain_versions: Dict[str, List[BaboonDomainVersion]] = {}

    def latest(self, domain: str) -> Version:
        versions = self.domain_versions.get(domain, [])
        if not versions:
            raise Exception(f"No registered version for '{domain}' domain found.")
        return versions[-1].version

    def register_facade(self, facade: 'BaboonCodecsFacade') -> None:
        self.domain_versions.update(facade.domain_versions)
        self.versions_codecs_json.update(facade.versions_codecs_json)
        self.versions_codecs_bin.update(facade.versions_codecs_bin)
        self.versions_conversions.update(facade.versions_conversions)
        self.versions_meta.update(facade.versions_meta)

    def register(self,
                 domain_version: BaboonDomainVersion,
                 codecs_json: Optional[Callable[[], AbstractBaboonJsonCodecs]] = None,
                 codecs_bin: Optional[Callable[[], AbstractBaboonUebaCodecs]] = None,
                 conversions: Optional[Callable[[], AbstractBaboonConversions]] = None,
                 meta: Optional[Callable[[], BaboonMeta]] = None) -> BaboonDomainVersion:
        self._register_version(domain_version)

        if codecs_json:
            self.versions_codecs_json[domain_version] = Lazy(codecs_json)
        if codecs_bin:
            self.versions_codecs_bin[domain_version] = Lazy(codecs_bin)
        if conversions:
            self.versions_conversions[domain_version] = Lazy(conversions)
        if meta:
            self.versions_meta[domain_version] = Lazy(meta)

        return domain_version

    def preload(self) -> None:
        try:
            [codec.value for codec in self.versions_codecs_json.values()]
            [codec.value for codec in self.versions_codecs_bin.values()]
        except Exception:
            pass

    def verify(self) -> None:
        if not self.domain_versions:
            raise Exception("Baboon codecs must have at least one domain registered.")

        for versions in self.domain_versions.values():
            for domain_version in versions:
                if domain_version not in self.versions_conversions:
                    raise BaboonCodecException.ConversionNotFound(
                        f"Baboon codecs must have conversion for {domain_version} registered."
                    )
                if domain_version not in self.versions_meta:
                    raise BaboonCodecException.CodecNotFound(
                        f"Baboon codecs must have codecs for {domain_version} registered."
                    )

    def try_convert(self, value: TI, target_type: Type[TO]) -> Optional[TO]:
        try:
            return self.convert(value, target_type)
        except Exception:
            return None

    def encode_to_bin(self,
                      ctx: str,
                      value: TI,
                      writer: Optional[LEDataOutputStream] = None,
                      type_meta_override: Optional[BaboonTypeMeta] = None) -> bytes:
        output_stream = writer or LEDataOutputStream(BytesIO())
        self._encode_to_bin_stream(ctx, output_stream, value, type_meta_override)
        return output_stream.stream.getvalue()

    def _encode_to_bin_stream(self,
                              ctx: str,
                              writer: LEDataOutputStream,
                              value: TI,
                              type_meta_override: Optional[BaboonTypeMeta] = None):
        type_meta = BaboonTypeMeta.from_instance(value)
        try:
            codec = self._get_bin_codec(type_meta, exact=True)
            meta = type_meta_override or type_meta
            meta.write_bin(writer)
            codec.encode(ctx, writer, value)
        except Exception as err:
            raise BaboonCodecException.EncoderFailure(
                f"Exception while trying to encode to binary form type "
                f"[{type_meta.domain_version}.{type_meta.type_identifier}] "
                f"of version '{type_meta.domain_version}'.",
                err
            )

    def decode_from_bin(self, reader: LEDataInputStream) -> BaboonGenerated:
        type_meta = BaboonTypeMeta.read_meta(reader)
        if not type_meta:
            raise BaboonCodecException.DecoderFailure("Cannot decode binary type meta")
        try:
            codec = self._get_bin_codec(type_meta, exact=False)
            return codec.decode(BaboonCodecContext.Compact, reader)
        except Exception as err:
            raise BaboonCodecException.DecoderFailure(
                f"Can not decode BIN form type "
                f"[{type_meta.domain_identifier}.{type_meta.type_identifier}] "
                f"of version '{type_meta.domain_version}'.",
                err
            )

    def decode_from_bin_bytes(self, data: bytes) -> BaboonGenerated:
        byte_stream = BytesIO(data)
        reader = LEDataInputStream(byte_stream)
        return self.decode_from_bin(reader)

    def decode_from_bin_latest(self, reader: LEDataInputStream, target_type: Type[TO]) -> TO:
        baboon = self.decode_from_bin(reader)
        return self.convert(baboon, target_type)

    def decode_from_bin_latest_bytes(self, data: bytes, target_type: Type[TO]) -> TO:
        byte_stream = BytesIO(data)
        reader = LEDataInputStream(byte_stream)
        baboon = self.decode_from_bin(reader)
        return self.convert(baboon, target_type)

    def encode_to_json(self,
                       value: BaboonGenerated,
                       type_meta_override: Optional[BaboonTypeMeta] = None) -> dict[str, Any]:
        type_meta = BaboonTypeMeta.from_instance(value)
        try:
            codec = self._get_json_codec(type_meta, exact=True)
            model_dict = codec.encode(BaboonCodecContext.Compact, value)
            meta_json = BaboonTypeMetaCodec.write_json(type_meta_override or type_meta)
            meta_json[self.CONTENT_JSON_KEY] = model_dict
            return model_dict
        except Exception as e:
            raise BaboonCodecException.EncoderFailure(
                f"Can not encode to json form type [{value.baboon_type_identifier}] "
                f"of version '{value.domain_version}'.",
                e
            )

    def encode_to_json_string(self,
                              value: BaboonGenerated,
                              type_meta_override: Optional[BaboonTypeMeta] = None) -> str:
        model_dict = self.encode_to_json(value, type_meta_override)
        return json.dumps(model_dict)

    def decode_from_json(self, value: dict[str, Any]) -> BaboonGenerated:
        try:
            type_meta = BaboonTypeMetaCodec.read_meta_json(value)
            codec = self._get_json_codec(type_meta, exact=False)
            content = dict[self.CONTENT_JSON_KEY]
            return codec.decode(content)
        except Exception as e:
            raise BaboonCodecException.DecoderFailure(
                f"Can not decode JSON form type "
                f"[{type_meta.domain_version}.{type_meta.type_identifier}] "
                f"of version '{type_meta.domain_version}'. JSON: {value}",
                e
            )

    def decode_from_json_string(self, value: str) -> BaboonGenerated:
        json_obj = json.loads(value)
        return self.decode_from_json(json_obj)

    def decode_from_json_latest(self, value: str, target_type: Type[TO]) -> TO:
        baboon = self.decode_from_json_string(value)
        return self.convert(baboon, target_type)

    def convert(self, value: BaboonGenerated, target_type: Type[TO]) -> TO:
        if type(value) is target_type:
            return value

        domain_version = value.domain_version

        versions = self.domain_versions.get(domain_version.domain_identifier, [])
        if not versions:
            raise BaboonCodecException.ConverterFailure(
                f"Unknown domain '{domain_version.domain_identifier}'."
            )

        if domain_version not in versions:
            raise BaboonCodecException.ConverterFailure(
                f"Unknown domain version '{domain_version}'."
            )

        converted = self._try_convert(value, versions)

        if type(converted) is not target_type:
            raise BaboonCodecException.ConverterFailure(
                f"Expected to have type [{target_type.__name__}] at the end, "
                f"but got [{type(converted).__name__}]."
            )

        return converted

    def _try_convert(self, value: BaboonGenerated, versions: List[BaboonDomainVersion]) -> BaboonGeneratedLatest:
        from_model = value

        for to_version in versions:
            if from_model.domain_version.version >= to_version.version:
                continue

            conversions = self.versions_conversions.get(to_version)
            if not conversions:
                raise BaboonCodecException.ConverterFailure(
                    f"Can not find version '{to_version}' conversions."
                )

            type_conversions = conversions.value.find_conversions(from_model)

            valid_conversions = [
                conv for conv in type_conversions
                if (type(from_model) == conv.type_from or
                    (isinstance(from_model, BaboonAdtMemberMeta) and
                     from_model.baboon_adt_type == conv.type_from))
            ]

            if not valid_conversions:
                raise BaboonCodecException.ConverterFailure(
                    f"Can not find version '{to_version}' type [{type(from_model).__name__}] conversions."
                )

            conversion = max(valid_conversions, key=lambda c: Version.from_str(c.version_to))

            try:
                from_model = conversions.value.convert(from_model, conversion)
            except Exception as e:
                raise BaboonCodecException.ConverterFailure(
                    f"Exception while converting type [{type(from_model).__name__}] "
                    f"of version '{from_model.domain_version}' to version '{to_version}'.",
                    e
                )

        return from_model

    def _get_bin_codec(self, type_meta: BaboonTypeMeta, exact: bool) -> BaboonBinCodec:
        return self._get_codec(self.versions_codecs_bin, type_meta, exact)

    def _get_json_codec(self, type_meta: BaboonTypeMeta, exact: bool) -> BaboonJsonCodec:
        return self._get_codec(self.versions_codecs_json, type_meta, exact)

    def _get_codec(self,
                   versions_codecs: Dict[BaboonDomainVersion, Lazy],
                   type_meta: BaboonTypeMeta,
                   exact: bool) -> BaboonCodecData:
        versions = self.domain_versions.get(type_meta.domain_identifier, [])
        if not versions:
            raise BaboonCodecException.CodecNotFound(
                f"Unknown domain {type_meta.domain_identifier}."
            )

        min_version = versions[0]
        max_version = versions[-1]

        # Determine model version
        if type_meta.version_min_compat and type_meta.version.version > max_version.version:
            model_version = BaboonDomainVersion(
                type_meta.domain_identifier,
                type_meta.version_min_compat.version
            )
        else:
            model_version = BaboonDomainVersion(
                type_meta.domain_identifier,
                type_meta.version.version
            )

        # Get appropriate codec
        if exact and model_version.version == max_version.version:
            return self._get_codec_exact(versions_codecs, model_version, type_meta.type_identifier)
        elif min_version.version <= model_version.version < max_version.version:
            return self._get_codec_max_compat(versions_codecs, model_version, max_version, type_meta.type_identifier)
        elif model_version.version < min_version.version:
            return self._get_codec_max_compat(versions_codecs, min_version, max_version, type_meta.type_identifier)
        else:
            raise BaboonCodecException.CodecNotFound(
                f"Unsupported domain version '{model_version}'."
            )

    def _get_codec_exact(self,
                         versions_codecs: Dict[BaboonDomainVersion, Lazy],
                         domain_version: BaboonDomainVersion,
                         type_identifier: str) -> Tuple[Optional[Exception], Optional[BaboonCodecData]]:
        """Get exact codec for version."""
        lazy_codecs = versions_codecs.get(domain_version)
        if not lazy_codecs:
            raise BaboonCodecException.CodecNotFound(
                f"No codecs registered for domain version '{domain_version}'."
            )

        codec_lazy = lazy_codecs.value.try_find(type_identifier)
        if not codec_lazy:
            raise BaboonCodecException.CodecNotFound(
                f"No codec found for type [{domain_version.domain_version}.{type_identifier}] "
                f"of version '{domain_version.version}'."
            )

        return codec_lazy.value

    def _get_codec_max_compat(self,
                              versions_codecs: Dict[BaboonDomainVersion, Lazy],
                              model_version: BaboonDomainVersion,
                              max_version: BaboonDomainVersion,
                              type_identifier: str) -> BaboonCodecData:
        """Get maximum compatible codec."""
        lazy_meta = self.versions_meta.get(model_version)
        if not lazy_meta:
            raise BaboonCodecException.CodecNotFound(
                f"Unknown domain version '{model_version}'."
            )

        same_versions = lazy_meta.value.same_in_versions(type_identifier)

        # Find max compat version
        max_compat = None
        for same_version in reversed(same_versions):
            if same_version == max_version.domain_version or Version.from_str(same_version) <= max_version.version:
                max_compat = BaboonDomainVersion(model_version.domain_identifier, same_version)
                break

        if not max_compat:
            raise BaboonCodecException.CodecNotFound(
                f"No max compat codec found for type "
                f"[{model_version.domain_identifier}.{type_identifier}] "
                f"of version '{model_version.version}'."
            )

        return self._get_codec_exact(versions_codecs, max_compat, type_identifier)

    def _register_version(self, domain_version: BaboonDomainVersion) -> None:
        domain_id = domain_version.domain_identifier
        if domain_id not in self.domain_versions:
            self.domain_versions[domain_id] = [domain_version]
        else:
            versions = self.domain_versions[domain_id]
            if domain_version not in versions:
                versions.append(domain_version)
                versions.sort(key=lambda v: v.version.version)