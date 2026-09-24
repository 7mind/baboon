package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{toSnakeCase, toSnakeCaseRaw}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BaboonLang, Domain, DomainMember, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import io.septimalmind.baboon.translator.DomainEnquiries

trait RsCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    rsRef: RsValue.RsType,
    srcRef: RsValue.RsType,
  ): Option[TextTree[RsValue]]
}

object RsCodecTestsTranslator {
  final class Impl(
    codecs: Set[RsCodecTranslator],
    domainEnquiries: DomainEnquiries,
    target: RsTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends RsCodecTestsTranslator {
    override def translate(
      definition: DomainMember.User,
      rsRef: RsValue.RsType,
      srcRef: RsValue.RsType,
    ): Option[TextTree[RsValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if domainEnquiries.hasForeignType(d, BaboonLang.Rust) => None
        case d if domainEnquiries.isRecursiveTypedef(d)              => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef]          => None
        case _ if !isLatestVersion                                     => None
        case _ =>
          val tests = makeTests(definition, srcRef)
          if (tests.isEmpty) None else Some(tests)
      }
    }

    private def makeTests(definition: DomainMember.User, srcRef: RsValue.RsType): TextTree[RsValue] = {
      val testFnName        = toSnakeCaseRaw(srcRef.name)
      val fixtureMethod     = fixtureMethodName(definition)
      val fixtureMethodJson = s"${fixtureMethod}_json"

      // PR-07-D01 (Rust analog): per-codec fixture variants. UEBA codec test uses the
      // `AnyOpaqueUeba`-bearing fixture (`random_*`); JSON codec test uses the
      // `AnyOpaqueJson`-bearing fixture (`random_*_json`). Each fixture matches its codec's
      // native any-field branch so round-trip avoids cross-format conversion and never needs a
      // `BaboonCodecContext::with_facade` ctx.
      val jsonTests = codecs
        .filter(_.isActive(definition.id)).collect {
          case _: RsJsonCodecGenerator =>
            val jsonRoundTrip = definition.defn match {
              case _: Typedef.Adt =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |let fixtures = super::${fixtureMethodJson}_all(&mut rnd);
                   |for fixture in fixtures {
                   |    let json = serde_json::to_value(&fixture).expect("JSON encode failed");
                   |    let decoded: $srcRef = serde_json::from_value(json.clone()).expect("JSON decode failed");
                   |    assert_eq!(fixture, decoded);
                   |}""".stripMargin
              case _: Typedef.Enum =>
                q"""for fixture in $srcRef::all() {
                   |    let json = serde_json::to_value(&fixture).expect("JSON encode failed");
                   |    let decoded: $srcRef = serde_json::from_value(json.clone()).expect("JSON decode failed");
                   |    assert_eq!(fixture, decoded);
                   |}""".stripMargin
              case _ =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |let fixture = super::$fixtureMethodJson(&mut rnd);
                   |let json = serde_json::to_value(&fixture).expect("JSON encode failed");
                   |let decoded: $srcRef = serde_json::from_value(json.clone()).expect("JSON decode failed");
                   |assert_eq!(fixture, decoded);""".stripMargin
            }

            // Conversion oracle: while the serde derive is still in place it defines the JSON
            // shape, so the explicit codecs must agree with it in both directions. This is what
            // lets the derives be removed later without moving the wire. The decode side is
            // compared against serde on the SAME wire rather than merely round-tripped, so a
            // shared misreading cannot cancel itself out.
            val encodeMatchesDerive = definition.defn match {
              case _: Typedef.Adt =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |for fixture in super::${fixtureMethodJson}_all(&mut rnd) {
                   |    let explicit = fixture.encode_json(&crate::baboon_runtime::BaboonCodecContext::Compact).expect("encode_json failed");
                   |    let derived = serde_json::to_value(&fixture).expect("serde encode failed");
                   |    assert_eq!(explicit, derived);
                   |}""".stripMargin
              case _: Typedef.Enum =>
                q"""for fixture in $srcRef::all() {
                   |    let explicit = fixture.encode_json(&crate::baboon_runtime::BaboonCodecContext::Compact).expect("encode_json failed");
                   |    let derived = serde_json::to_value(&fixture).expect("serde encode failed");
                   |    assert_eq!(explicit, derived);
                   |}""".stripMargin
              case _ =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |let fixture = super::$fixtureMethodJson(&mut rnd);
                   |let explicit = fixture.encode_json(&crate::baboon_runtime::BaboonCodecContext::Compact).expect("encode_json failed");
                   |let derived = serde_json::to_value(&fixture).expect("serde encode failed");
                   |assert_eq!(explicit, derived);""".stripMargin
            }

            val decodeMatchesDerive = definition.defn match {
              case _: Typedef.Adt =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |for fixture in super::${fixtureMethodJson}_all(&mut rnd) {
                   |    let wire = serde_json::to_value(&fixture).expect("serde encode failed");
                   |    let explicit = $srcRef::decode_json(&crate::baboon_runtime::BaboonCodecContext::Compact, &wire).expect("decode_json failed");
                   |    let derived: $srcRef = serde_json::from_value(wire).expect("serde decode failed");
                   |    assert_eq!(explicit, derived);
                   |    assert_eq!(explicit, fixture);
                   |}""".stripMargin
              case _: Typedef.Enum =>
                q"""for fixture in $srcRef::all() {
                   |    let wire = serde_json::to_value(&fixture).expect("serde encode failed");
                   |    let explicit = $srcRef::decode_json(&crate::baboon_runtime::BaboonCodecContext::Compact, &wire).expect("decode_json failed");
                   |    let derived: $srcRef = serde_json::from_value(wire).expect("serde decode failed");
                   |    assert_eq!(explicit, derived);
                   |    assert_eq!(explicit, fixture);
                   |}""".stripMargin
              case _ =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |let fixture = super::$fixtureMethodJson(&mut rnd);
                   |let wire = serde_json::to_value(&fixture).expect("serde encode failed");
                   |let explicit = $srcRef::decode_json(&crate::baboon_runtime::BaboonCodecContext::Compact, &wire).expect("decode_json failed");
                   |let derived: $srcRef = serde_json::from_value(wire).expect("serde decode failed");
                   |assert_eq!(explicit, derived);
                   |assert_eq!(explicit, fixture);""".stripMargin
            }

            q"""#[test]
               |fn test_${testFnName}_json_codec() {
               |    crate::cross_language_fixture_path::assert_cross_language_fixture_root_exists();
               |    for _ in 0..${target.generic.codecTestIterations.toString} {
               |        ${jsonRoundTrip.shift(8).trim}
               |    }
               |}
               |
               |#[test]
               |fn test_${testFnName}_json_cross_language() {
               |    crate::cross_language_fixture_path::assert_cross_language_fixture_root_exists();
               |    let tpeid = "${definition.id.render}";
               |    let path_string = crate::cross_language_fixture_path::cross_language_fixture_path("cs", &format!("{}.json", tpeid), "json-default");
               |    let path = std::path::Path::new(&path_string);
               |    if !path.exists() {
               |        eprintln!("Skipping cross-language test: {:?} not found", path);
               |        return;
               |    }
               |    let data = std::fs::read_to_string(path).expect("Failed to read JSON file");
               |    let decoded: $srcRef = serde_json::from_str(&data).expect("Failed to decode cross-language JSON");
               |    let re_encoded = serde_json::to_value(&decoded).expect("Failed to re-encode");
               |    let re_decoded: $srcRef = serde_json::from_value(re_encoded).expect("Failed to decode re-encoded");
               |    assert_eq!(decoded, re_decoded);
               |}
               |
               |#[test]
               |fn test_${testFnName}_json_encode_matches_derive() {
               |    for _ in 0..${target.generic.codecTestIterations.toString} {
               |        ${encodeMatchesDerive.shift(8).trim}
               |    }
               |}
               |
               |#[test]
               |fn test_${testFnName}_json_decode_matches_derive() {
               |    for _ in 0..${target.generic.codecTestIterations.toString} {
               |        ${decodeMatchesDerive.shift(8).trim}
               |    }
               |}""".stripMargin
        }.toList

      val uebaTests = codecs
        .filter(_.isActive(definition.id)).collect {
          case _: RsUEBACodecGenerator =>
            val uebaRoundTrip = definition.defn match {
              case _: Typedef.Adt =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |let fixtures = super::${fixtureMethod}_all(&mut rnd);
                   |for fixture in fixtures {
                   |    let mut buf = Vec::new();
                   |    crate::baboon_runtime::BaboonBinEncode::encode_ueba(&fixture, &ctx, &mut buf).expect("UEBA encode failed");
                   |    let mut cursor = std::io::Cursor::new(&buf);
                   |    let decoded = <$srcRef as crate::baboon_runtime::BaboonBinDecode>::decode_ueba(&ctx, &mut cursor).expect("UEBA decode failed");
                   |    assert_eq!(fixture, decoded);
                   |}""".stripMargin
              case _: Typedef.Enum =>
                q"""for fixture in $srcRef::all() {
                   |    let mut buf = Vec::new();
                   |    crate::baboon_runtime::BaboonBinEncode::encode_ueba(&fixture, &ctx, &mut buf).expect("UEBA encode failed");
                   |    let mut cursor = std::io::Cursor::new(&buf);
                   |    let decoded = <$srcRef as crate::baboon_runtime::BaboonBinDecode>::decode_ueba(&ctx, &mut cursor).expect("UEBA decode failed");
                   |    assert_eq!(fixture, decoded);
                   |}""".stripMargin
              case _ =>
                q"""let mut rnd = crate::baboon_fixture::BaboonRandom::new();
                   |let fixture = super::$fixtureMethod(&mut rnd);
                   |let mut buf = Vec::new();
                   |crate::baboon_runtime::BaboonBinEncode::encode_ueba(&fixture, &ctx, &mut buf).expect("UEBA encode failed");
                   |let mut cursor = std::io::Cursor::new(&buf);
                   |let decoded = <$srcRef as crate::baboon_runtime::BaboonBinDecode>::decode_ueba(&ctx, &mut cursor).expect("UEBA decode failed");
                   |assert_eq!(fixture, decoded);""".stripMargin
            }

            q"""#[test]
               |fn test_${testFnName}_ueba_codec() {
               |    crate::cross_language_fixture_path::assert_cross_language_fixture_root_exists();
               |    let ctx = crate::baboon_runtime::BaboonCodecContext::Default;
               |    for _ in 0..${target.generic.codecTestIterations.toString} {
               |        ${uebaRoundTrip.shift(8).trim}
               |    }
               |}""".stripMargin
        }.toList

      (jsonTests ++ uebaTests).joinNN()
    }

    private def fixtureMethodName(definition: DomainMember.User): String = {
      s"random_${toSnakeCaseRaw(definition.id.name.name)}"
    }
  }
}
