package io.septimalmind.baboon

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
            content <- F.maybeSuspend(IzFiles.readString(lockfilePath.toPath))
            parsed  <- F.fromEither(io.circe.parser.parse(content))
            out     <- F.fromEither(parsed.as[Locks])
          } yield {
            out
          }).catchAll(e => F.fail(BaboonIssue.of(IOIssue.CantReadInput(lockfilePath.toString, e))))
          _ <- compareSigs(model, currentSigs, existingSigs)
        } yield {}

      case Some(lockfilePath) =>
        F.maybeSuspend(IzFiles.writeUtfString(lockfilePath.toPath, currentSigs.asJson.spaces2))
      case None =>
        F.unit
    }
  }

  private def compareSigs(model: BaboonFamily, currentSigs: Locks, existingSigs: Locks): F[NEList[BaboonIssue], Unit] = {
    val ci        = index(currentSigs)
    val ei        = index(existingSigs)
    val different = ci.filter { case (k, id) => ei.contains(k) && !ei.get(k).contains(id) }.filterNot { case ((pkg, v), _) => model.domains(pkg).evolution.latest == v }
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
