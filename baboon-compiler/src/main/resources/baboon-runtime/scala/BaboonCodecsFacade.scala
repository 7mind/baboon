package baboon.runtime.shared {

  import io.circe.Json

  import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
  import scala.collection.concurrent.TrieMap
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scala.reflect.ClassTag
  import scala.util.Try

  trait BaboonCodecsFacade {
    type BaboonValue[T] = Either[BaboonCodecException, T]

    private val CONTENT_JSON_KEY = "$c"

    private val versionsCodecsJson: TrieMap[BaboonDomainVersion, Lazy[AbstractBaboonJsonCodecs]]   = TrieMap.empty
    private val versionsCodecsBin: TrieMap[BaboonDomainVersion, Lazy[AbstractBaboonUebaCodecs]]    = TrieMap.empty
    private val versionsConversions: TrieMap[BaboonDomainVersion, Lazy[AbstractBaboonConversions]] = TrieMap.empty
    private val versionsMeta: TrieMap[BaboonDomainVersion, Lazy[BaboonMeta]]                       = TrieMap.empty

    private val domainVersions: TrieMap[String, List[BaboonDomainVersion]] = TrieMap.empty

    def latest(domain: String): Version = {
      domainVersions.get(domain).filter(_.nonEmpty) match {
        case Some(versions) => versions.last.version
        case None           => throw new Exception(s"No registered version for $domain domain found.")
      }
    }

    def register(facade: BaboonCodecsFacade): Unit = {
      facade.domainVersions.foreach { case (id, versions) => domainVersions.put(id, versions) }
      facade.versionsCodecsJson.foreach { case (id, codec) => versionsCodecsJson.put(id, codec) }
      facade.versionsCodecsBin.foreach { case (id, codec) => versionsCodecsBin.put(id, codec) }
      facade.versionsConversions.foreach { case (id, conversion) => versionsConversions.put(id, conversion) }
      facade.versionsMeta.foreach { case (id, meta) => versionsMeta.put(id, meta) }
    }

    def register(
      domainVersion: BaboonDomainVersion,
      codecsJson: => AbstractBaboonJsonCodecs,
      codecsBin: => AbstractBaboonUebaCodecs,
      conversions: => AbstractBaboonConversions,
      meta: => BaboonMeta,
    ): BaboonDomainVersion = {
      register(domainVersion)
      versionsCodecsJson.put(domainVersion, Lazy(codecsJson))
      versionsCodecsBin.put(domainVersion, Lazy(codecsBin))
      versionsConversions.put(domainVersion, Lazy(conversions))
      versionsMeta.put(domainVersion, Lazy(meta))
      domainVersion
    }

    def register(
      domainVersion: BaboonDomainVersion,
      codecsJson: => AbstractBaboonJsonCodecs,
      codecsBin: => AbstractBaboonUebaCodecs,
    ): BaboonDomainVersion = {
      register(domainVersion)
      versionsCodecsJson.put(domainVersion, Lazy(codecsJson))
      versionsCodecsBin.put(domainVersion, Lazy(codecsBin))
      domainVersion
    }

    def register(
      domainVersion: BaboonDomainVersion,
      codecsJson: => AbstractBaboonJsonCodecs,
      codecsBin: => AbstractBaboonUebaCodecs,
      meta: => BaboonMeta,
    ): BaboonDomainVersion = {
      register(domainVersion)
      versionsCodecsJson.put(domainVersion, Lazy(codecsJson))
      versionsCodecsBin.put(domainVersion, Lazy(codecsBin))
      versionsMeta.put(domainVersion, Lazy(meta))
      domainVersion
    }

    def register(
      domainVersion: BaboonDomainVersion,
      conversions: => AbstractBaboonConversions,
    ): BaboonDomainVersion = {
      register(domainVersion)
      versionsConversions.put(domainVersion, Lazy(conversions))
      domainVersion
    }

    def register(
      domainVersion: BaboonDomainVersion,
      meta: => BaboonMeta,
    )(implicit d: DummyImplicit
    ): BaboonDomainVersion = {
      register(domainVersion)
      versionsMeta.put(domainVersion, Lazy(meta))
      domainVersion
    }

    def preload(): Unit = {
      Future {
        versionsCodecsJson.values.foreach(_.value)
        versionsCodecsBin.values.foreach(_.value)
      }.recover(_ => ())
    }

    def verify(): Unit = {
      if (domainVersions.isEmpty) {
        throw new Exception("Baboon codecs must have at least one domain registered.")
      }

      domainVersions.flatMap(_._2).foreach {
        domainVersion =>
          if (!versionsConversions.contains(domainVersion)) {
            throw BaboonCodecException.ConversionNotFound(s"Baboon codecs must have conversion for $domainVersion registered.")
          }
          if (!versionsMeta.contains(domainVersion)) {
            throw BaboonCodecException.CodecNotFound(s"Baboon codecs must have codecs for $domainVersion registered.")
          }
      }
    }

    def tryConvert[TI <: BaboonGenerated, TO <: BaboonGeneratedLatest](value: TI): Option[TO] = {
      convert(value).toOption
    }

    def encodeToBin[T <: BaboonGenerated: ClassTag](
      ctx: BaboonCodecContext,
      value: T,
    ): BaboonValue[Array[Byte]] = {
      encodeToBin(ctx, value, None)
    }

    def encodeToBin[T <: BaboonGenerated: ClassTag](
      ctx: BaboonCodecContext,
      value: T,
      typeMetaOverride: Option[BaboonTypeMeta],
    ): BaboonValue[Array[Byte]] = {
      val byteStream = new ByteArrayOutputStream()
      val ledStream  = new LEDataOutputStream(byteStream)
      encodeToBin(ctx, ledStream, value, typeMetaOverride).map(_ => byteStream.toByteArray)
    }

    def encodeToBin[T <: BaboonGenerated: ClassTag](
      ctx: BaboonCodecContext,
      writer: LEDataOutputStream,
      value: T,
    ): BaboonValue[Unit] = {
      encodeToBin(ctx, writer, value, None)
    }

    def encodeToBin[T <: BaboonGenerated: ClassTag](
      ctx: BaboonCodecContext,
      writer: LEDataOutputStream,
      value: T,
      typeMetaOverride: Option[BaboonTypeMeta],
    ): BaboonValue[Unit] = {
      val typeMeta = BaboonTypeMeta.from(value)
      (for {
        codec <- getBinCodec(typeMeta, exact = true).toTry
        _     <- Try(typeMetaOverride.getOrElse(typeMeta).writeBin(writer))
        _     <- Try(codec.encode(ctx, writer, value))
      } yield ()).toEither.left.map(
        e =>
          BaboonCodecException.EncoderFailure(
            s"Exception while trying to encode to binary form type [${typeMeta.domainVersion}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
            e,
          )
      )
    }

    def decodeFromBin(reader: LEDataInputStream): BaboonValue[BaboonGenerated] = {
      for {
        typeMeta <- BaboonTypeMeta
          .readMeta(reader)
          .toRight(BaboonCodecException.DecoderFailure("Cannot decode binary type meta"))

        codec <- getBinCodec(typeMeta, exact = false)
        baboon <- codec
          .decode(BaboonCodecContext.Compact, reader).left.map(
            e =>
              BaboonCodecException.DecoderFailure(
                s"Can not decode BIN form type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
                e,
              )
          )
      } yield baboon
    }

    def decodeFromBin(bytes: Array[Byte]): BaboonValue[BaboonGenerated] = {
      val byteInputStream = new ByteArrayInputStream(bytes)
      val ledInputStream  = new LEDataInputStream(byteInputStream)
      decodeFromBin(ledInputStream)
    }

    def decodeFromBinLatest(bytes: Array[Byte]): BaboonValue[BaboonGeneratedLatest] = {
      decodeFromBin(bytes).flatMap(baboon => convert[BaboonGenerated, BaboonGeneratedLatest](baboon))
    }

    def decodeFromBinLatest[T <: BaboonGeneratedLatest: ClassTag](reader: LEDataInputStream): BaboonValue[T] = {
      decodeFromBin(reader).flatMap(baboon => convert[BaboonGenerated, T](baboon))
    }

    def decodeFromBinLatest[T <: BaboonGeneratedLatest: ClassTag](bytes: Array[Byte]): BaboonValue[T] = {
      decodeFromBin(bytes).flatMap(baboon => convert[BaboonGenerated, T](baboon))
    }

    def encodeToJson[T <: BaboonGenerated: ClassTag](value: T, typeMetaOverride: Option[BaboonTypeMeta]): BaboonValue[Json] = {
      val typeMeta = BaboonTypeMeta.from(value)
      (for {
        jsonCodec  <- getJsonCodec(typeMeta, exact = true).toTry
        jsonContent = jsonCodec.encode(BaboonCodecContext.Compact, value)
        metaJson    = typeMetaOverride.getOrElse(typeMeta).writeJson
        result      = metaJson.mapObject(_.add(CONTENT_JSON_KEY, jsonContent))
      } yield result).toEither.left.map(
        e =>
          BaboonCodecException.EncoderFailure(
            s"Can not encode to json form type [${value.baboonTypeIdentifier}] of version '${value.domainVersion}'.",
            e,
          )
      )
    }

    def encodeToJson[T <: BaboonGenerated: ClassTag](value: T): BaboonValue[Json] = {
      encodeToJson(value, None)
    }

    def encodeToJsonString[T <: BaboonGenerated: ClassTag](value: T, typeMetaOverride: Option[BaboonTypeMeta]): BaboonValue[String] = {
      encodeToJson(value, typeMetaOverride).map(_.noSpaces)
    }

    def encodeToJsonString[T <: BaboonGenerated: ClassTag](value: T): BaboonValue[String] = {
      encodeToJsonString(value, None)
    }

    def decodeFromJson(value: Json): BaboonValue[Option[BaboonGenerated]] = {
      // This must be a proper, previously serialized baboon object, keep it as is.
      val maybeTypeMeta     = BaboonTypeMeta.readMeta(value)
      val maybeContentToken = value.hcursor.downField(CONTENT_JSON_KEY).as[Json].toOption
      (maybeTypeMeta, maybeContentToken) match {
        case (Some(typeMeta), Some(contentToken)) =>
          Try(getJsonCodec(typeMeta, exact = false).flatMap(_.decode(BaboonCodecContext.Compact, contentToken))).toEither.left
            .map(
              e =>
                BaboonCodecException.DecoderFailure(
                  s"Can not decode JSON form type [${typeMeta.domainVersion}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'. JSON: $value",
                  e,
                )
            ).map(_.toOption)
        case _ => Right(None)
      }
    }

    def decodeFromJson(value: String): BaboonValue[Option[BaboonGenerated]] = {
      val json = io.circe.parser.parse(value).toOption.get
      decodeFromJson(json)
    }

    def decodeFromJsonLatest(value: String): BaboonValue[Option[BaboonGenerated]] = {
      decodeFromJson(value).flatMap(
        _.fold[Either[BaboonCodecException, Option[BaboonGenerated]]](Right(None))(v => convert[BaboonGenerated, BaboonGeneratedLatest](v).map(Some(_)))
      )
    }

    def decodeFromJsonLatest[T <: BaboonGeneratedLatest: ClassTag](value: String): BaboonValue[Option[T]] = {
      decodeFromJson(value).flatMap(
        _.fold[Either[BaboonCodecException, Option[T]]](Right(None))(v => convert[BaboonGenerated, T](v).map(Some(_)))
      )
    }

    def decodeFromJsonLatest[T <: BaboonGeneratedLatest: ClassTag](value: Json): BaboonValue[Option[T]] = {
      decodeFromJson(value).flatMap(
        _.fold[Either[BaboonCodecException, Option[T]]](Right(None))(v => convert[BaboonGenerated, T](v).map(Some(_)))
      )
    }

    def convert[From <: BaboonGenerated, TO <: BaboonGeneratedLatest](
      value: From
    )(implicit ctTo: ClassTag[TO]
    ): BaboonValue[TO] = {
      def isAdtConversion(instance: BaboonGenerated, conversion: Conversion): Boolean = {
        instance match {
          case adt: BaboonAdtMemberMeta => adt.baboonAdtType == conversion.typeFrom
          case _                        => false
        }
      }

      def tryConvert(versions: List[BaboonDomainVersion]): BaboonValue[BaboonGenerated] = {
        // iterate from first to the latest version, trying to apply conversions
        versions.foldLeft[Either[BaboonCodecException, BaboonGenerated]](Right(value)) {
          case (fromModel, toVersion) =>
            if (fromModel.exists(_.domainVersion.version >= toVersion.version)) {
              // our model version is higher or the same as this one
              fromModel
            } else {
              for {
                fromModel       <- fromModel
                lazyConversions <- versionsConversions.get(toVersion).toRight(BaboonCodecException.ConverterFailure(s"Can not find version '$toVersion' conversions."))
                typeConversions  = lazyConversions.value.findConversions(fromModel)
                maybeConversion = typeConversions
                  .filter(typeConversion => fromModel.getClass == typeConversion.typeFrom || isAdtConversion(fromModel, typeConversion))
                  .maxByOption(conversion => Version.from(conversion.versionTo))
                conversion <- maybeConversion.toRight(
                  BaboonCodecException.ConverterFailure(s"Can not find version '$toVersion' type [${fromModel.getClass.getName}] conversions.")
                )
                result <- Try(lazyConversions.value.convert(fromModel, conversion)).toEither.left.map(
                  e =>
                    BaboonCodecException.ConverterFailure(
                      s"Exception while converting type [${fromModel.getClass.getName}] of version '${fromModel.domainVersion}' to version '$toVersion'.",
                      e,
                    )
                )
              } yield result
            }
        }
      }

      value match {
        case to: TO => Right(to)
        case _ =>
          val domainVersion = value.domainVersion
          for {
            versions <- domainVersions
              .get(domainVersion.domainIdentifier)
              .filter(_.nonEmpty)
              .toRight(BaboonCodecException.ConverterFailure(s"Unknown domain '${domainVersion.domainIdentifier}'."))

            _         <- versions.find(_ == domainVersion).toRight(BaboonCodecException.ConverterFailure(s"Unknown domain version' $domainVersion'."))
            converted <- tryConvert(versions)
            result <- converted match {
              case io: TO => Right(io)
              case _ =>
                Left(
                  BaboonCodecException.ConverterFailure(
                    s"Expected to have type [${ctTo.runtimeClass.getName}] at the end, but got [${converted.getClass.getName}]."
                  )
                )
            }
          } yield result
      }
    }

    private def getBinCodec(typeMeta: BaboonTypeMeta, exact: Boolean): BaboonValue[BaboonBinCodec[BaboonGenerated]] = {
      getCodec(versionsCodecsBin, typeMeta, exact).map(_.asInstanceOf[BaboonBinCodec[BaboonGenerated]])
    }

    private def getJsonCodec(typeMeta: BaboonTypeMeta, exact: Boolean): BaboonValue[BaboonJsonCodec[BaboonGenerated]] = {
      getCodec(versionsCodecsJson, typeMeta, exact).map(_.asInstanceOf[BaboonJsonCodec[BaboonGenerated]])
    }

    private def getCodec[TCodecs <: AbstractBaboonCodecs](
      versionsCodecs: TrieMap[BaboonDomainVersion, Lazy[TCodecs]],
      typeMeta: BaboonTypeMeta,
      exact: Boolean,
    ): BaboonValue[BaboonCodecData] = {
      for {
        versions <- domainVersions
          .get(typeMeta.domainIdentifier)
          .filter(_.nonEmpty)
          .toRight(BaboonCodecException.CodecNotFound(s"Unknown domain ${typeMeta.domainIdentifier}."))

        minVersion = versions.head
        maxVersion = versions.last
        // extract domain model version and latest version
        modelVersion = typeMeta.versionMinCompat match {
          // it's a model of newer version than we have, we should find min compat version
          case Some(v) if typeMeta.version.version > maxVersion.version => v
          case _                                                        => typeMeta.version
        }

        codec <- modelVersion match {
          // it's a model of latest version, get last version codec
          case v if exact && v.version == maxVersion.version =>
            getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier)
          // it's a model of outdated version, we should read it with max compat codec
          case v if v.version >= minVersion.version && v.version < maxVersion.version =>
            getCodecMaxCompat(versionsCodecs, modelVersion, maxVersion, typeMeta.typeIdentifier)
          // it's a model of deprecated version, we should try to decode with minimal version
          case v if v.version < minVersion.version =>
            getCodecMaxCompat(versionsCodecs, minVersion, maxVersion, typeMeta.typeIdentifier)
          case _ => Left(BaboonCodecException.CodecNotFound(s"Unsupported domain version '$modelVersion'."))
        }
      } yield codec
    }

    private def getCodecExact[TCodecs <: AbstractBaboonCodecs](
      versionsCodecs: TrieMap[BaboonDomainVersion, Lazy[TCodecs]],
      domainVersion: BaboonDomainVersion,
      typeIdentifier: String,
    ): BaboonValue[BaboonCodecData] = {
      for {
        lazyVersionCodecs <- versionsCodecs
          .get(domainVersion)
          .toRight(BaboonCodecException.CodecNotFound(s"No codecs registered for domain version '$domainVersion'."))
        codec <- lazyVersionCodecs.value
          .tryFind(typeIdentifier)
          .map(_.value)
          .toRight(BaboonCodecException.CodecNotFound(s"No codec found for type [${domainVersion.domainVersion}.$typeIdentifier] of version '${domainVersion.version}'."))
      } yield codec
    }

    private def getCodecMaxCompat[TCodecs <: AbstractBaboonCodecs](
      versionsCodecs: TrieMap[BaboonDomainVersion, Lazy[TCodecs]],
      modelVersion: BaboonDomainVersion,
      maxVersion: BaboonDomainVersion,
      typeIdentifier: String,
    ): BaboonValue[BaboonCodecData] = {
      (for {
        lazyDomainMeta <- versionsMeta
          .get(modelVersion)
          .toRight(BaboonCodecException.CodecNotFound(s"Unknown domain version '{$modelVersion}'."))

        // version is registered, and we can read it with max compat codec
        // scan same versions from max to min, to find max compat version
        sameVersions = lazyDomainMeta.value.sameInVersions(typeIdentifier)
        maybeCodec = sameVersions
          .findLast(sameVersion => sameVersion == maxVersion.domainVersion || Version.from(sameVersion) <= maxVersion.version).map {
            sameVersion =>
              val maxCompatVersion = BaboonDomainVersion(modelVersion.domainIdentifier, sameVersion)
              getCodecExact(versionsCodecs, maxCompatVersion, typeIdentifier)
          }
        codec <- maybeCodec.toRight(
          BaboonCodecException.CodecNotFound(
            s"No max compat codec found for type [${modelVersion.domainIdentifier}.{$typeIdentifier}] of version '${modelVersion.version}'."
          )
        )
      } yield codec).flatten
    }

    private def register(domainVersion: BaboonDomainVersion): BaboonDomainVersion = {
      domainVersions
        .updateWith(domainVersion.domainIdentifier) {
          case Some(versions) => Some(if (versions.contains(domainVersion)) versions else versions.appended(domainVersion).sortBy(_.version))
          case None           => Some(List(domainVersion))
        }
      domainVersion
    }
  }
}
