package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{toSnakeCase, toSnakeCaseRaw}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

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
    typeTranslator: RsTypeTranslator,
    enquiries: BaboonEnquiries,
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
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ if !isLatestVersion                            => None
        case _ =>
          val tests = makeTests(definition, srcRef)
          if (tests.isEmpty) None else Some(tests)
      }
    }

    private def makeTests(definition: DomainMember.User, srcRef: RsValue.RsType): TextTree[RsValue] = {
      val testFnName    = toSnakeCaseRaw(srcRef.name)
      val fixtureMethod = fixtureMethodName(definition)

      val jsonTests = codecs
        .filter(_.isActive(definition.id)).collect {
          case _: RsJsonCodecGenerator =>
            val jsonRoundTrip = definition.defn match {
              case _: Typedef.Adt =>
                q"""let fixtures = super::${fixtureMethod}_all(&mut rnd);
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
                q"""let fixture = super::$fixtureMethod(&mut rnd);
                   |let json = serde_json::to_value(&fixture).expect("JSON encode failed");
                   |let decoded: $srcRef = serde_json::from_value(json.clone()).expect("JSON decode failed");
                   |assert_eq!(fixture, decoded);""".stripMargin
            }

            q"""#[test]
               |fn test_${testFnName}_json_codec() {
               |    for _ in 0..${target.generic.codecTestIterations.toString} {
               |        let mut rnd = crate::baboon_fixture::BaboonRandom::new();
               |        ${jsonRoundTrip.shift(8).trim}
               |    }
               |}
               |
               |#[test]
               |fn test_${testFnName}_json_cross_language() {
               |    let tpeid = "${definition.id.render}";
               |    let path = format!("./../target/cs/json-default/{}.json", tpeid);
               |    let path = std::path::Path::new(&path);
               |    if !path.exists() {
               |        eprintln!("Skipping cross-language test: {:?} not found", path);
               |        return;
               |    }
               |    let data = std::fs::read_to_string(path).expect("Failed to read JSON file");
               |    let decoded: $srcRef = serde_json::from_str(&data).expect("Failed to decode cross-language JSON");
               |    let re_encoded = serde_json::to_value(&decoded).expect("Failed to re-encode");
               |    let re_decoded: $srcRef = serde_json::from_value(re_encoded).expect("Failed to decode re-encoded");
               |    assert_eq!(decoded, re_decoded);
               |}""".stripMargin
        }.toList

      val uebaTests = codecs
        .filter(_.isActive(definition.id)).collect {
          case _: RsUEBACodecGenerator =>
            val uebaRoundTrip = definition.defn match {
              case _: Typedef.Adt =>
                q"""let fixtures = super::${fixtureMethod}_all(&mut rnd);
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
                q"""let fixture = super::$fixtureMethod(&mut rnd);
                   |let mut buf = Vec::new();
                   |crate::baboon_runtime::BaboonBinEncode::encode_ueba(&fixture, &ctx, &mut buf).expect("UEBA encode failed");
                   |let mut cursor = std::io::Cursor::new(&buf);
                   |let decoded = <$srcRef as crate::baboon_runtime::BaboonBinDecode>::decode_ueba(&ctx, &mut cursor).expect("UEBA decode failed");
                   |assert_eq!(fixture, decoded);""".stripMargin
            }

            q"""#[test]
               |fn test_${testFnName}_ueba_codec() {
               |    let ctx = crate::baboon_runtime::BaboonCodecContext::Default;
               |    for _ in 0..${target.generic.codecTestIterations.toString} {
               |        let mut rnd = crate::baboon_fixture::BaboonRandom::new();
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
