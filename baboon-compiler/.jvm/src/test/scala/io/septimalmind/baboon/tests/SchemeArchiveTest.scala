package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model.{FSPath, InputPointer, RawInclude, RawNodeMeta}
import io.septimalmind.baboon.parser.{BaboonArchiveInputs, BaboonParser}
import io.septimalmind.baboon.scheme.*
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.{Crc32, StoredZipReader}
import io.septimalmind.baboon.{Baboon, BaboonLoader}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.TimeZone
import java.util.zip.{ZipEntry, ZipOutputStream}

final class SchemeArchiveTest extends BaboonTest[Either] {
  import SchemeArchiveTest.*

  private val Evo    = Pkg(NEList("zipdemo", "evo"))
  private val Revert = Pkg(NEList("zipdemo", "revert"))
  private val Shapes = Pkg(NEList("zipdemo", "shapes"))

  private def fixtureRoot: Path = IzResources.getPath("scheme-zip-ok").get.asInstanceOf[IzResources.LoadablePathReference].path

  private def load(loader: BaboonLoader[Either]): BaboonFamily = {
    val files = IzFiles.walk(fixtureRoot.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    loader.load(files).fold(issues => fail(s"fixture failed to load: $issues"), identity)
  }

  private def select(family: BaboonFamily, selectors: String): Either[NEList[String], NEList[SchemeDomainVersion]] = {
    SchemeSelection.parseSelectors(selectors).flatMap(SchemeSelection.resolve(family, _))
  }

  private def archive(renderer: BaboonSchemeRenderer, family: BaboonFamily, selectors: String): Array[Byte] = {
    (for {
      selection <- select(family, selectors)
      entries   <- SchemeArchive.render(renderer, family, selection)
    } yield SchemeZipWriter.toBytes(entries)).fold(e => fail(e.toList.mkString("\n")), identity)
  }

  private def reload(manager: BaboonFamilyManager[Either], bytes: Array[Byte]): BaboonFamily = {
    val inputs = BaboonArchiveInputs.fromZip(bytes).fold(e => fail(e.toList.mkString("\n")), identity)
    manager.load(inputs.models.toList).fold(issues => fail(s"archive failed to reload: $issues"), identity)
  }

  private def versionsOf(family: BaboonFamily): Set[(String, String)] = {
    family.domains.toMap.toList.flatMap { case (pkg, lineage) => lineage.versions.toMap.keys.map(v => (pkg.toString, v.toString)) }.toSet
  }

  private def sameIn(family: BaboonFamily, pkg: Pkg, version: String, name: String): List[String] = {
    val id = TypeId.User(pkg, Owner.Toplevel, TypeName(name))
    family.domains.toMap(pkg).evolution.typesUnchangedSince(Version.parse(version))(id).sameIn.toList.map(_.toString)
  }

  private def renamesOf(family: BaboonFamily, pkg: Pkg): Map[String, Map[String, String]] = {
    family.domains.toMap(pkg).evolution.diffs.map {
      case (step, diff) => step.toString -> diff.changes.renamed.map { case (n, o) => n.toString -> o.toString }
    }
  }

  "scheme selection" should {
    "resolve every selector form, unioning and deduplicating overlaps" in {
      (loader: BaboonLoader[Either]) =>
        val family                                 = load(loader)
        def names(selectors: String): List[String] = select(family, selectors).fold(e => fail(e.toList.mkString), _.toList.map(SchemeArchive.entryPath))
        val all = List(
          "schemas/zipdemo.evo/1.0.0.baboon",
          "schemas/zipdemo.evo/2.0.0.baboon",
          "schemas/zipdemo.evo/3.0.0.baboon",
          "schemas/zipdemo.revert/1.0.0.baboon",
          "schemas/zipdemo.revert/2.0.0.baboon",
          "schemas/zipdemo.revert/3.0.0.baboon",
          "schemas/zipdemo.shapes/1.0.0.baboon",
          "schemas/zipdemo.shapes/2.0.0.baboon",
        )
        assert(names("*@*") == all)
        assert(names("zipdemo.evo@*") == all.take(3))
        assert(names("zipdemo.evo@1.0.0,zipdemo.evo@2.0.0") == all.take(2))
        assert(names("*@1.0.0") == List(all(0), all(3), all(6)))
        assert(names("zipdemo.evo@*, *@1.0.0 ,zipdemo.evo@1.0.0,*@*") == all)
        assert(names("zipdemo.evo@*,*@1.0.0") == all.take(4) :+ all(6))
    }

    "reject selectors that match nothing, naming what is available" in {
      (loader: BaboonLoader[Either]) =>
        val family = load(loader)
        for (selector <- List("no.such@*", "zipdemo.evo@9.0.0", "*@9.9.9", "zipdemo.evo@*,no.such@1.0.0")) {
          val message = select(family, selector).fold(_.toList.mkString("\n"), s => fail(s"$selector selected $s"))
          assert(message.contains("matches no loaded domain version"), message)
          assert(message.contains("zipdemo.evo@{1.0.0, 2.0.0, 3.0.0}; zipdemo.revert@{1.0.0, 2.0.0, 3.0.0}; zipdemo.shapes@{1.0.0, 2.0.0}"), message)
        }
    }

    "reject a selection that skips a version, because reloading it would fabricate evolution" in {
      (loader: BaboonLoader[Either]) =>
        val family  = load(loader)
        val message = select(family, "zipdemo.evo@1.0.0,zipdemo.evo@3.0.0,zipdemo.revert@*,zipdemo.revert@2.0.0").fold(_.toList.mkString("\n"), s => fail(s"selected $s"))
        assert(message.contains("selection of zipdemo.evo (1.0.0, 3.0.0) skips 2.0.0"), message)
        assert(!message.contains("zipdemo.revert"), message)
        val revert = select(family, "zipdemo.revert@1.0.0,*@3.0.0").fold(_.toList.mkString("\n"), s => fail(s"selected $s"))
        assert(revert.contains("selection of zipdemo.revert (1.0.0, 3.0.0) skips 2.0.0"), revert)
    }
  }

  "the gap a non-contiguous selection would open" should {
    "either fail to reload or fabricate evolution (why gaps are rejected)" in {
      (loader: BaboonLoader[Either], manager: BaboonFamilyManager[Either], renderer: BaboonSchemeRenderer) =>
        val family = load(loader)
        def gapped(pkg: Pkg): Array[Byte] = {
          SchemeZipWriter.toBytes(
            SchemeArchive
              .render(renderer, family, NEList(SchemeDomainVersion(pkg, Version.parse("1.0.0")), SchemeDomainVersion(pkg, Version.parse("3.0.0"))))
              .fold(e => fail(e.toString), identity)
          )
        }
        // 3.0.0 declares `New : was[Mid]`, and Mid exists only in the omitted 2.0.0
        val renamed = BaboonArchiveInputs.fromZip(gapped(Evo)).fold(e => fail(e.toString), identity)
        assert(manager.load(renamed.models.toList).left.toOption.get.toList.map(_.toString) == List("Evolution(InvalidTypeRename(zipdemo.evo/:#New,zipdemo.evo/:#Mid))"))
        // Flip changed in 2.0.0 and reverted in 3.0.0; the gapped reload claims 3.0.0 bytes are 1.0.0 bytes
        val reverted = reload(manager, gapped(Revert))
        assert(sameIn(family, Revert, "3.0.0", "Flip") == List("3.0.0"))
        assert(sameIn(reverted, Revert, "3.0.0", "Flip") == List("1.0.0", "3.0.0"))
    }
  }

  "a scheme archive" should {
    "be byte-identical across runs and time zones, with stored entries, a fixed timestamp and no extra fields" in {
      (loader: BaboonLoader[Either], renderer: BaboonSchemeRenderer) =>
        val family   = load(loader)
        val first    = archive(renderer, family, "*@*")
        val original = TimeZone.getDefault
        val shifted =
          try {
            TimeZone.setDefault(TimeZone.getTimeZone("Pacific/Kiritimati"))
            archive(renderer, load(loader), "zipdemo.shapes@*,zipdemo.revert@*,zipdemo.evo@*")
          } finally TimeZone.setDefault(original)
        assert(java.util.Arrays.equals(first, shifted))

        val entries = StoredZipReader.read(first).fold(fail(_), identity)
        assert(entries.map(_.path) == entries.map(_.path).sorted)
        assert(entries.forall(!_.isDirectory))
        // first local header: method 0, DOS time 00:00:00, DOS date 1980-01-01, no extra field
        assert(u16(first, 8) == 0 && u16(first, 10) == 0 && u16(first, 12) == 0x0021 && u16(first, 28) == 0)
    }

    "reload every contiguous selection with its schemas and evolution intact" in {
      (loader: BaboonLoader[Either], manager: BaboonFamilyManager[Either], renderer: BaboonSchemeRenderer) =>
        val family = load(loader)
        for (selectors <- List("*@*", "zipdemo.evo@2.0.0,zipdemo.evo@3.0.0", "zipdemo.evo@3.0.0", "*@1.0.0", "zipdemo.shapes@*", "zipdemo.revert@2.0.0,*@3.0.0")) {
          val selection = select(family, selectors).fold(e => fail(e.toString), identity)
          val reloaded  = reload(manager, archive(renderer, family, selectors))
          assert(versionsOf(reloaded) == selection.toList.map(dv => (dv.pkg.toString, dv.version.toString)).toSet, selectors)
          selection.toList.foreach {
            dv =>
              assert(renderer.render(reloaded, dv.pkg, dv.version) == renderer.render(family, dv.pkg, dv.version), s"$selectors: ${dv.pkg}@${dv.version}")
          }
        }

        val all = reload(manager, archive(renderer, family, "*@*"))
        assert(renamesOf(all, Evo) == renamesOf(family, Evo))
        assert(renamesOf(all, Shapes) == renamesOf(family, Shapes))
        assert(renamesOf(family, Evo).values.flatten.toSet == Set("zipdemo.evo/:#Mid" -> "zipdemo.evo/:#Old", "zipdemo.evo/:#New" -> "zipdemo.evo/:#Mid"))
        for (v <- List("1.0.0", "2.0.0", "3.0.0")) assert(sameIn(all, Revert, v, "Flip") == sameIn(family, Revert, v, "Flip"))
        assert(sameIn(all, Shapes, "2.0.0", "Order") == sameIn(family, Shapes, "2.0.0", "Order"))
        assert(
          all.domains.toMap(Shapes).evolution.minReaders(Version.parse("2.0.0"), TypeId.User(Shapes, Owner.Toplevel, TypeName("Order"))) ==
          family.domains.toMap(Shapes).evolution.minReaders(Version.parse("2.0.0"), TypeId.User(Shapes, Owner.Toplevel, TypeName("Order")))
        )

        // a tail selection keeps the step it contains and the first version's own rename declaration
        val tail = reload(manager, archive(renderer, family, "zipdemo.evo@2.0.0,zipdemo.evo@3.0.0"))
        assert(renamesOf(tail, Evo) == Map("2.0.0->3.0.0" -> Map("zipdemo.evo/:#New" -> "zipdemo.evo/:#Mid")))
        assert(tail.domains.toMap(Evo).versions.toMap(Version.parse("2.0.0")).renames.map {
          case (n, o) => n.toString -> o.toString
        } == Map("zipdemo.evo/:#Mid" -> "zipdemo.evo/:#Old"))
        val revertTail = reload(manager, archive(renderer, family, "zipdemo.revert@2.0.0,zipdemo.revert@3.0.0"))
        assert(sameIn(revertTail, Revert, "3.0.0", "Flip") == List("3.0.0"))
    }
  }

  "the :scheme archive entrypoint" should {
    "publish the archive atomically and nothing at all on failure" in {
      (loader: BaboonLoader[Either], renderer: BaboonSchemeRenderer) =>
        import izumi.distage.modules.support.unsafe.EitherSupport.{quasiIOEither, quasiIORunnerEither}
        import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

        val dirs   = Set(FSPath.parse(NEString.unsafeFrom(fixtureRoot.toFile.getCanonicalPath)))
        val out    = Files.createTempDirectory("scheme-archive-")
        val target = out.resolve("nested/schemas.zip")
        def run(selectors: String): Either[NEList[String], Unit] = {
          Baboon.schemeArchiveEntrypoint(dirs, Set.empty, SchemeSelection.parseSelectors(selectors).fold(e => fail(e.toString), identity), target.toString)
        }

        assert(run("*@*") == Right(()))
        val written = Files.readAllBytes(target)
        assert(java.util.Arrays.equals(written, archive(renderer, load(loader), "*@*")))

        assert(run("zipdemo.evo@1.0.0,zipdemo.evo@3.0.0").isLeft)
        assert(run("zipdemo.revert@1.0.0,zipdemo.revert@3.0.0").isLeft)
        assert(run("no.such@*").isLeft)
        assert(java.util.Arrays.equals(Files.readAllBytes(target), written), "a failed run replaced the published archive")
        assert(target.getParent.toFile.list().toList == List("schemas.zip"), "temporary files were left behind")

        val fresh = out.resolve("fresh/schemas.zip")
        assert(Baboon.schemeArchiveEntrypoint(dirs, Set.empty, SchemeSelection.parseSelectors("*@4.0.0").toOption.get, fresh.toString).isLeft)
        assert(!Files.exists(fresh))
    }
  }

  "archive input reading" should {
    "reject archives that are not stored ZIPs within the contract" in {
      val model                              = "model a.b\n\nversion \"1.0.0\"\n\nroot data X {\n  x: i32\n}\n".getBytes(StandardCharsets.UTF_8)
      val good                               = RawEntry("schemas/a.b/1.0.0.baboon".getBytes(StandardCharsets.UTF_8), model, Utf8Flag, Stored, None)
      def errors(bytes: Array[Byte]): String = BaboonArchiveInputs.fromZip(bytes).fold(_.toList.mkString("\n"), r => fail(s"accepted: $r"))

      assert(BaboonArchiveInputs.fromZip(build(List(good))).isRight)
      assert(errors(Array.emptyByteArray).contains("not a ZIP archive"))
      assert(errors("PK not really".getBytes(StandardCharsets.UTF_8)).contains("not a ZIP archive"))
      assert(errors(build(List(good)).dropRight(1)).contains("not a ZIP archive"))
      assert(errors(deflated("schemas/a.b/1.0.0.baboon", model)).contains("compression method 8"))
      assert(errors(build(List(good.copy(crc = Some(0L))))).contains("CRC-32"))
      assert(errors(build(List(good.copy(flags = Utf8Flag | 0x0001)))).contains("encrypted"))
      assert(errors(build(List(good.copy(name = Array(0x61, 0xFF, 0x2E, 0x62).map(_.toByte))))).contains("not valid UTF-8"))
      assert(errors(build(List(good.copy(name = "\u00e9.baboon".getBytes(StandardCharsets.UTF_8), flags = 0)))).contains("without the UTF-8 flag"))
      assert(errors(zip64(build(List(good)))).contains("ZIP64"))

      for (unsafe <- List("../x.baboon", "/x.baboon", "a\\b.baboon", "C:/x.baboon", "a//b.baboon", "./x.baboon", "a/./b.baboon", "a/../b.baboon")) {
        assert(errors(build(List(good, good.copy(name = unsafe.getBytes(StandardCharsets.UTF_8))))).contains(s"unsafe archive entry path '$unsafe'"), unsafe)
      }
      assert(errors(build(List(good, good))).contains("duplicate archive entry path 'schemas/a.b/1.0.0.baboon'"))
      assert(errors(build(List(good, good.copy(name = "README.md".getBytes(StandardCharsets.UTF_8))))).contains("unsupported archive entry 'README.md'"))
      assert(errors(build(List(good.copy(data = Array(0xC3, 0x28).map(_.toByte))))).contains("is not valid UTF-8"))
      assert(errors(build(List(good.copy(name = "inc/defs.bmo".getBytes(StandardCharsets.UTF_8))))).contains("contains no *.baboon schema"))
      assert(errors(build(Nil)).contains("contains no *.baboon schema"))

      val withDirectory = BaboonArchiveInputs
        .fromZip(build(List(good.copy(name = "schemas/".getBytes(StandardCharsets.UTF_8), data = Array.emptyByteArray), good))).fold(e => fail(e.toString), identity)
      assert(withDirectory.models.toList.map(_.path.asString) == List("schemas/a.b/1.0.0.baboon") && withDirectory.includables.isEmpty)
    }

    "resolve includes against the archive root only" in {
      val defs                                  = BaboonParser.Input(FSPath.parse(NEString.unsafeFrom("shared/defs.bmo")), "data D {}")
      val model                                 = BaboonParser.Input(FSPath.parse(NEString.unsafeFrom("schemas/a/1.0.0.baboon")), "model a")
      val resolver                              = new BaboonArchiveInputs.ArchiveInclusionResolver[Either](List(defs, model))
      def resolve(path: String): Option[String] = resolver.resolveInclude(RawInclude(RawNodeMeta(InputPointer.Undefined), path)).map(_._1.asString)
      assert(resolve("shared/defs.bmo") == Some("shared/defs.bmo"))
      assert(resolve("./shared//defs.bmo") == Some("shared/defs.bmo"))
      assert(resolve("schemas/../shared/defs.bmo") == Some("shared/defs.bmo"))
      assert(resolve("../shared/defs.bmo").isEmpty)
      assert(resolve("/shared/defs.bmo").isEmpty)
      assert(resolve("defs.bmo").isEmpty)
    }
  }
}

object SchemeArchiveTest {
  val Utf8Flag: Int = 0x0800
  val Stored: Int   = 0

  final case class RawEntry(name: Array[Byte], data: Array[Byte], flags: Int, method: Int, crc: Option[Long])

  def u16(bytes: Array[Byte], offset: Int): Int = (bytes(offset) & 0xFF) | ((bytes(offset + 1) & 0xFF) << 8)

  /** A minimal ZIP writer that, unlike `java.util.zip`, also writes the malformed archives the reader must reject. */
  def build(entries: List[RawEntry]): Array[Byte] = {
    val out                                           = new ByteArrayOutputStream()
    val central                                       = new ByteArrayOutputStream()
    def le16(o: ByteArrayOutputStream, v: Int): Unit  = { o.write(v & 0xFF); o.write((v >>> 8) & 0xFF) }
    def le32(o: ByteArrayOutputStream, v: Long): Unit = { le16(o, (v & 0xFFFF).toInt); le16(o, ((v >>> 16) & 0xFFFF).toInt) }
    entries.foreach {
      e =>
        val offset = out.size().toLong
        val crc    = e.crc.getOrElse(Crc32.of(e.data))
        le32(out, 0x04034B50L); le16(out, 10); le16(out, e.flags); le16(out, e.method); le16(out, 0); le16(out, 0x21)
        le32(out, crc); le32(out, e.data.length.toLong); le32(out, e.data.length.toLong); le16(out, e.name.length); le16(out, 0)
        out.write(e.name); out.write(e.data)
        le32(central, 0x02014B50L); le16(central, 0x031E); le16(central, 10); le16(central, e.flags); le16(central, e.method); le16(central, 0); le16(central, 0x21)
        le32(central, crc); le32(central, e.data.length.toLong); le32(central, e.data.length.toLong); le16(central, e.name.length)
        le16(central, 0); le16(central, 0); le16(central, 0); le16(central, 0); le32(central, 0L); le32(central, offset)
        central.write(e.name)
    }
    val directoryOffset = out.size().toLong
    out.write(central.toByteArray)
    le32(out, 0x06054B50L); le16(out, 0); le16(out, 0); le16(out, entries.size); le16(out, entries.size)
    le32(out, central.size().toLong); le32(out, directoryOffset); le16(out, 0)
    out.toByteArray
  }

  def deflated(name: String, data: Array[Byte]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val zip = new ZipOutputStream(out)
    zip.putNextEntry(new ZipEntry(name))
    zip.write(data)
    zip.closeEntry()
    zip.close()
    out.toByteArray
  }

  /** Marks the end-of-central-directory entry counts as ZIP64-deferred. */
  def zip64(bytes: Array[Byte]): Array[Byte] = {
    val patched = bytes.clone()
    val eocd    = patched.length - 22
    Seq(8, 9, 10, 11).foreach(i => patched(eocd + i) = 0xFF.toByte)
    patched
  }
}
