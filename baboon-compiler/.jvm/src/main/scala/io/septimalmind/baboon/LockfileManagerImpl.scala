package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, IOIssue, VerificationIssue}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain, Pkg, Version}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.crypto.IzSha256HashFunction
import izumi.fundamentals.platform.files.IzFiles
import PathTools.*

class LockfileManagerImpl[F[+_, +_]: Error2: MaybeSuspend2](
  options: CompilerOptions,
  enq: BaboonEnquiries,
) extends LockfileManager[F] {
  def validateLock(model: BaboonFamily): F[NEList[BaboonIssue], Unit] = {
    val currentSigs = Locks(model.domains.map {
      case (pkg, lineage) =>
        val versions = lineage.versions.toSeq.map {
          case (ver, dom) =>
            VersionLock(ver, sigOf(dom))
        }.toList
        (pkg, versions)
    }.toMap)

    // `evolution.latest` is the only model-derived input the enforcement matrix needs,
    // so we project it out and drive the matrix over the projection. This keeps the
    // enforcement-then-update control flow testable without constructing a full BaboonFamily.
    enforce(currentSigs, pkg => model.domains(pkg).evolution.latest)
  }

  /** Enforcement-then-update over the present/absent × update × enforcement matrix.
    *
    * Sequencing is load-bearing: enforcement runs BEFORE update in the for-comprehension so a
    * failing enforcement short-circuits and prevents the `Force` rewrite (force must not silence
    * a violation).
    */
  private[baboon] def enforce(currentSigs: Locks, latestOf: Pkg => Version): F[NEList[BaboonIssue], Unit] = {
    import LockCodecs.*
    import io.circe.syntax.*

    def writeCurrent(lockfilePath: FSPath): F[NEList[BaboonIssue], Unit] =
      F.maybeSuspend(IzFiles.writeUtfString(lockfilePath.toPath, currentSigs.asJson.spaces2))

    options.lockFile match {
      case None =>
        F.unit

      case Some(lockfilePath) if !lockfilePath.toFile.exists() =>
        // ABSENT file: nothing to enforce against; CreateOnly and Force both write currentSigs.
        options.lockfileUpdate match {
          case LockfileUpdate.CreateOnly => writeCurrent(lockfilePath)
          case LockfileUpdate.Force      => writeCurrent(lockfilePath)
        }

      case Some(lockfilePath) =>
        // PRESENT file: read+parse, then ENFORCE (authoritative) BEFORE applying the update.
        for {
          existingSigs <- (for {
            content <- F.maybeSuspend(IzFiles.readString(lockfilePath.toPath))
            parsed  <- F.fromEither(io.circe.parser.parse(content))
            out     <- F.fromEither(parsed.as[Locks])
          } yield {
            out
          }).catchAll(e => F.fail(BaboonIssue.of(IOIssue.CantReadInput(lockfilePath.toString, e))))
          _ <- compareSigs(latestOf, currentSigs, existingSigs, options.lockfileEnforcement)
          // Reached only when enforcement passed (or was None): force rewrites, create-only leaves untouched.
          _ <- options.lockfileUpdate match {
            case LockfileUpdate.Force      => writeCurrent(lockfilePath)
            case LockfileUpdate.CreateOnly => F.unit
          }
        } yield {}
    }
  }

  private def compareSigs(
    latestOf: Pkg => Version,
    currentSigs: Locks,
    existingSigs: Locks,
    enforcement: LockfileEnforcement,
  ): F[NEList[BaboonIssue], Unit] = {
    val ci    = index(currentSigs)
    val ei    = index(existingSigs)
    val drift = ci.filter { case (k, id) => ei.contains(k) && !ei.get(k).contains(id) }

    val different = enforcement match {
      case LockfileEnforcement.None =>
        // No check at all.
        Map.empty[(Pkg, Version), SigId]
      case LockfileEnforcement.LegacyVersions =>
        // Flag non-latest drift only — the latest version is permitted to drift.
        drift.filterNot { case ((pkg, v), _) => latestOf(pkg) == v }
      case LockfileEnforcement.AllVersions =>
        // Flag every drifted (pkg, version), including the latest.
        drift
    }

    F.ifThenFail(different.nonEmpty)(NEList.unsafeFrom(different.map { case ((pkg, v), _) => VerificationIssue.LockedVersionModified(pkg, v): BaboonIssue }.toList))
  }

  private def sigOf(dom: Domain): SigId = {
    val repr = dom.typeMeta.view.toSeq.map {
      case (id, meta) =>
        s"[${enq.wrap(id)}:${meta.deepId.id}]"
    }.mkString("\n")
    SigId(IzSha256HashFunction.hash(repr))
  }

  private def index(currentSigs: Locks): Map[(Pkg, Version), SigId] = {
    currentSigs.locks.flatMap {
      case (p, v) =>
        v.map(v => ((p, v.version), v.sigId))
    }.toMap
  }

}
