package io.septimalmind.baboon.bincompat

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.git.GitModelMaterializer
import io.septimalmind.baboon.git.GitModelMaterializer.MaterializeFailure
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{Domain, Pkg, Version}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.Path

/** Resolve the `:bincompat` `--from` / `--to` refs to TWO INDEPENDENT [[Domain]] values (T190/G34).
  *
  * This is the from/to -> Domain resolution SEAM. It exists BECAUSE the `:diff` fast path funnels
  * both sides through a single Version-keyed lineage map (`versions = lineage.versions.toMap`,
  * keyed by [[Version]]); two Domains that carry the SAME version number but come from DIFFERENT
  * commits (the headline `--from 3.0.0@HEAD~3 --to 3.0.0` case) would COLLIDE in that map — only one
  * would survive. This seam BYPASSES the shared map entirely: each side is loaded into its OWN
  * family+lineage and the requested [[Domain]] is extracted from THAT side's lineage, so equal
  * version ids on the two sides never merge.
  *
  * The two Domains it yields are fed DIRECTLY to [[BincompatClassifier.classify]] (which takes two
  * Domains and is NOT version-key-sensitive). This seam does NOT route through `evolve()` /
  * `EvolutionStep(prev.version, last.version)` / `toUniqueMap` — those collide on equal version ids
  * (see `BaboonComparator.scala`), which is exactly the collision this seam avoids.
  *
  * IO shape mirrors T195's `Baboon.diffEntrypointPerSide` per-side load pattern: a BARE side (no
  * `@ref`) walks the working-tree `--model-dir`/`--model` inputs directly; an `@ref` side runs
  * [[GitModelMaterializer]] (git pre-flight + detached worktree) and walks the re-rooted paths
  * INSIDE the materializer bracket, so the worktree stays alive for the duration of the load.
  *
  * Unlike the diff entrypoint, this seam does NOT call `sys.exit`: it is a reusable resolution
  * function, so every failure mode is surfaced in the typed [[BincompatResolver.ResolveFailure]]
  * error channel. The entrypoint wiring (T191) owns the exit-code mapping.
  */
final class BincompatResolver[F[+_, +_]: Error2: MaybeSuspend2](
  materializer: GitModelMaterializer[F]
) {
  import BincompatResolver.*

  /** Resolve both sides and return `(fromDomain, toDomain)` as two DISTINCT [[Domain]] values.
    *
    * @param loader    the shared [[BaboonLoader]] (from the injector); reused for both sides.
    * @param pkg       the requested domain package.
    * @param fromRef   the parsed `--from` side (bare version, or version pinned at a git ref).
    * @param toRef     the parsed `--to` side.
    * @param modelDirs the working-tree `--model-dir` inputs (directories, NOT yet walked).
    * @param models    the working-tree `--model` inputs (individual `.baboon` files).
    * @param cwd       the process working directory; used to resolve relative inputs / refs.
    */
  def resolvePair(
    loader: BaboonLoader[F],
    pkg: Pkg,
    fromRef: VersionRefLike,
    toRef: VersionRefLike,
    modelDirs: List[Path],
    models: List[Path],
    cwd: Path,
  ): F[ResolveFailure, (Domain, Domain)] = {
    // Nest the two side-resolutions so an `@ref` side's throwaway worktree stays alive while THAT
    // side's Domain is loaded and extracted. Each side loads into its OWN family+lineage, so the two
    // Domains never share a Version-keyed map and equal version ids do not collide.
    withSidePaths(fromRef.ref, modelDirs, models, cwd) {
      fromPaths =>
        resolveDomain(loader, fromPaths, pkg, fromRef.version).flatMap {
          fromDomain =>
            withSidePaths(toRef.ref, modelDirs, models, cwd) {
              toPaths =>
                resolveDomain(loader, toPaths, pkg, toRef.version).map {
                  toDomain => (fromDomain, toDomain)
                }
            }
        }
    }
  }

  /** Resolve one side's `.baboon` inputs, then run `use`. A bare side walks the working tree
    * directly; an `@ref` side runs the materializer (git pre-flight + detached worktree) and walks
    * the re-rooted paths INSIDE the bracket so the worktree stays alive for `use`.
    *
    * The materializer's continuation is fixed to its OWN [[MaterializeFailure]] error channel, so a
    * [[ResolveFailure]] raised by `use` (a loader / domain / version failure) cannot be thrown out
    * of the bracket directly — it is reified into the VALUE channel (`Either[ResolveFailure, A]`)
    * inside the bracket and re-raised once the materializer has finished (worktree cleaned up). This
    * keeps the worktree alive for the whole load without conflating the two failure taxonomies. */
  private def withSidePaths[A](
    ref: Option[String],
    modelDirs: List[Path],
    models: List[Path],
    cwd: Path,
  )(use: List[Path] => F[ResolveFailure, A]): F[ResolveFailure, A] = {
    ref match {
      case None =>
        F.maybeSuspend(walkBaboon(modelDirs, models)).flatMap(use)
      case Some(r) =>
        materializer
          .withModelsAtRef(r, modelDirs, models, cwd) {
            materialized =>
              F.maybeSuspend(walkBaboon(materialized.modelDirs, materialized.models))
                .flatMap(paths => F.attempt(use(paths))) // reify ResolveFailure into the value channel
          }
          .catchAll(f => F.fail(ResolveFailure.Materialize(f): ResolveFailure))
          .flatMap(F.fromEither(_))
    }
  }

  /** Load a family from one side's paths and extract the requested version's [[Domain]] out of THAT
    * side's own lineage. Loader / domain-not-found / version-not-found each surface as a distinct
    * typed [[ResolveFailure]] — no `sys.exit`. */
  private def resolveDomain(
    loader: BaboonLoader[F],
    paths: List[Path],
    pkg: Pkg,
    version: Version,
  ): F[ResolveFailure, Domain] = {
    for {
      family <- loader.load(paths).leftMap(ResolveFailure.Loader(_): ResolveFailure)
      lineage <- family.domains.toMap.get(pkg) match {
        case Some(l) => F.pure(l)
        case None =>
          val available = family.domains.toMap.keys.map(_.path.mkString(".")).toList.sorted
          F.fail(ResolveFailure.DomainNotFound(pkg, available))
      }
      domain <- lineage.versions.toMap.get(version) match {
        case Some(d) => F.pure(d)
        case None =>
          val available = lineage.versions.toMap.keys.map(_.toString).toList.sorted
          F.fail(ResolveFailure.VersionNotFound(pkg, version, available))
      }
    } yield domain
  }

  /** Walk model directories + individual model files into the flat `.baboon` path list that
    * [[BaboonLoader.load]] expects — the same rule the diff seam's `walkBaboon` applies. */
  private def walkBaboon(modelDirs: List[Path], models: List[Path]): List[Path] = {
    val fromDirs = modelDirs.flatMap(dir => IzFiles.walk(dir.toFile).filter(_.toFile.getName.endsWith(".baboon")))
    (models ++ fromDirs).distinct
  }
}

object BincompatResolver {

  /** The minimal parsed-side shape this seam consumes — a [[Version]] plus an optional git ref.
    * Structurally identical to `io.septimalmind.baboon.diff.VersionRef`; kept as a small local
    * contract so the seam does not depend on the diff package and T191 can adapt either shape. */
  final case class VersionRefLike(version: Version, ref: Option[String])

  /** Typed resolution failures. The entrypoint (T191) maps each to an exit code / message; the seam
    * never exits the process itself. */
  sealed trait ResolveFailure
  object ResolveFailure {

    /** A git-side failure while materializing an `@ref` side (git missing, ref unresolvable, path
      * outside repo, absent at ref, worktree add/cleanup). Carries the materializer's own taxonomy so
      * the entrypoint can reuse `GitModelMaterializerErrors`. */
    final case class Materialize(failure: MaterializeFailure) extends ResolveFailure

    /** The loader failed to parse/type a side's `.baboon` inputs. */
    final case class Loader(issues: NEList[BaboonIssue]) extends ResolveFailure

    /** The requested domain package is not present in a side's loaded family. */
    final case class DomainNotFound(pkg: Pkg, available: List[String]) extends ResolveFailure

    /** The requested version is absent from a side's lineage for the requested domain. */
    final case class VersionNotFound(pkg: Pkg, version: Version, available: List[String]) extends ResolveFailure
  }
}
