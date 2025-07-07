package io.septimalmind.baboon

import io.circe.generic.semiauto.{deriveCodec, deriveEncoder}
import io.circe.{Codec, Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.{CantCleanupTarget, CantReadInput, LockedVersionModified}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain, Pkg, UnmodifiedSince, Version}
import io.septimalmind.baboon.util.{BLogger, BaboonMetagen}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.crypto.IzSha256HashFunction
import izumi.fundamentals.platform.files.IzFiles

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

case class SigId(value: String) extends AnyVal

case class VersionLock(
  version: Version,
  sigId: SigId,
)

case class Locks(
  locks: Map[Pkg, List[VersionLock]]
)

object LockCodecs {
  implicit lazy val pkgKeyEncoder: KeyEncoder[Pkg]       = KeyEncoder.encodeKeyString.contramap(_.toString)
  implicit lazy val pkgKeyDecoder: KeyDecoder[Pkg]       = KeyDecoder.decodeKeyString.map(s => Pkg(NEList.unsafeFrom(s.split('.').toList)))
  implicit lazy val versionCodec: Codec[Version]         = Codec.from(Decoder.decodeString.map(s => Version(s)), Encoder.encodeString.contramap(_.version))
  implicit lazy val sigidCodec: Codec[SigId]             = Codec.from(Decoder.decodeString.map(s => SigId(s)), Encoder.encodeString.contramap(_.value))
  implicit lazy val versionLockCodec: Codec[VersionLock] = deriveCodec
  implicit lazy val lockscodec: Codec[Locks]             = deriveCodec

}

trait BaboonCompiler[F[+_, +_]] {
  def run(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit]
}

object BaboonCompiler {
  class BaboonCompilerImpl[F[+_, +_]: Error2: MaybeSuspend2]( // dirty, I/O happens there
    translator: BaboonAbstractTranslator[F],
    options: CompilerOptions,
    logger: BLogger,
    metagen: BaboonMetagen,
    enq: BaboonEnquiries,
  ) extends BaboonCompiler[F] {

    override def run(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit] = {

      for {
        _ <- cleanupTargetPaths(target.output)
        _ <- writeEvolutionJson(target, model)
        _ <- validateLock(model)

        _          <- F.maybeSuspend(logger.message(s"${target.id}: generating output..."))
        translated <- translator.translate(model)

        _ <- F.traverseAccumErrors_(translated.files.iterator) {
          case (relativePath, output) =>
            F.traverse_(target.output.targetPathFor(output)) {
              targetDirectory =>
                val targetPath = targetDirectory.resolve(relativePath)
                writeFile(output, targetPath)
            }
        }
        _ <- F.maybeSuspend(logger.message(s"${target.id}: done"))

      } yield {}
    }

    // TODO: move this into a separate component
    private def validateLock(model: BaboonFamily): F[NEList[BaboonIssue], Unit] = {
      import LockCodecs.*
      import io.circe.syntax.*

      val currentSigs = Locks(model.domains.map {
        case (pkg, lineage) =>
          val versions = lineage.versions.toSeq.map {
            case (ver, dom) =>
              VersionLock(ver, sigOf(dom))
          }.toList
          (pkg, versions)
      }.toMap)

      options.lockFile match {
        case Some(lockfilePath) if lockfilePath.toFile.exists() =>
          for {
            existingSigs <- (for {
              content <- F.maybeSuspend(IzFiles.readString(lockfilePath))
              parsed  <- F.fromEither(io.circe.parser.parse(content))
              out     <- F.fromEither(parsed.as[Locks])
            } yield {
              out
            }).catchAll(e => F.fail(NEList(CantReadInput(lockfilePath.toString, e))))
            _ <- compareSigs(model, currentSigs, existingSigs)
          } yield {}

        case Some(lockfilePath) =>
          F.maybeSuspend(IzFiles.writeUtfString(lockfilePath, currentSigs.asJson.spaces2))
        case None =>
          F.unit
      }
    }

    private def compareSigs(model: BaboonFamily, currentSigs: Locks, existingSigs: Locks): F[NEList[BaboonIssue], Unit] = {
      val ci        = index(currentSigs)
      val ei        = index(existingSigs)
      val different = ci.filter { case (k, id) => ei.contains(k) && !ei.get(k).contains(id) }.filterNot { case ((pkg, v), _) => model.domains(pkg).evolution.latest == v }
      F.ifThenFail(different.nonEmpty)(NEList.unsafeFrom(different.map { case ((pkg, v), _) => LockedVersionModified(pkg, v) }.toList))
    }

    private def index(currentSigs: Locks): Map[(Pkg, Version), SigId] = {
      currentSigs.locks.flatMap {
        case (p, v) =>
          v.map(v => ((p, v.version), v.sigId))
      }.toMap
    }

    private def sigOf(dom: Domain): SigId = {
      val repr = dom.typeMeta.view.toSeq.map {
        case (id, meta) =>
          s"[${enq.wrap(id)}:${meta.deepId.id}]"
      }.mkString("\n")
      SigId(IzSha256HashFunction.hash(repr))
    }

    private def writeFile(content: OutputFile, tgt: Path): F[NEList[BaboonIssue], Unit] = {
      F.fromAttempt {
        tgt.getParent.toFile.mkdirs()

        if (options.debug) {
          logger.message("debug", s"$tgt\n$content")
        }

        Files.writeString(
          tgt,
          content.content.replace("\r", ""),
          StandardCharsets.UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING,
        )
        ()
      }.leftMap(t => NEList(BaboonIssue.CantWriteOutput(tgt.toString, t)))
    }

    private def cleanupTargetPaths(targetOptions: OutputOptions): F[NEList[BaboonIssue], Unit] = {
      for {
        targetPaths <- F.pure(targetOptions.targetPaths.values.toList.distinct.filter(_.toFile.exists()))
        unexpectedFiles <- F.fromAttempt {
          targetPaths.flatMap {
            IzFiles.walk(_).filter {
              p =>
                val f = p.toFile
                !f.isDirectory && !(
                  f.getName.startsWith(".") ||
                  targetOptions.safeToRemoveExtensions.exists(ext => f.getName.endsWith(s".$ext"))
                )
            }
          }
        }.catchAll(t => F.fail(NEList(CantCleanupTarget(Seq.empty, targetOptions.safeToRemoveExtensions.toSeq, Some(t)))))
        _ <- F
          .ifThenElse(unexpectedFiles.isEmpty)(
            F.fromAttempt(targetPaths.foreach(path => IzFiles.erase(path)))
              .catchAll(t => F.fail(NEList(CantCleanupTarget(unexpectedFiles.map(_.toString), targetOptions.safeToRemoveExtensions.toSeq, Some(t))))),
            F.fail(NEList(CantCleanupTarget(unexpectedFiles.map(_.toString), targetOptions.safeToRemoveExtensions.toSeq, None))),
          )

      } yield {}
    }

    private def writeEvolutionJson(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit] = {
      F.traverse_(options.metaWriteEvolutionJsonTo) {
        maybePath =>
          val path = Option(maybePath.getParent) match {
            case Some(_) => maybePath
            case None    => target.output.output.resolve(maybePath)
          }

          val result = metagen.meta(model).toString()

          writeFile(OutputFile(result.replace("\r", ""), CompilerProduct.CustomMeta), path)
      }
    }
  }

}
