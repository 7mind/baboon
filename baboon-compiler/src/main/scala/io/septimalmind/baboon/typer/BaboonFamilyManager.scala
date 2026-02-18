package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.parser.model.{FSPath, RawContent, RawDomain, RawNodeMeta, RawTLDef, RawTypeName}
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonFamilyCache, BaboonLineage, Domain, DomainKey}
import io.septimalmind.baboon.util.{BLogger, FileContentProvider}
import io.septimalmind.baboon.validator.BaboonValidator
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import izumi.fundamentals.graphs.struct.AdjacencyPredList
import izumi.fundamentals.graphs.{DAG, GraphMeta}
import izumi.fundamentals.platform.strings.TextTree.Quote

import scala.collection.mutable

trait BaboonFamilyManager[F[+_, +_]] {
  def load(
    definitions: List[BaboonParser.Input]
  ): F[NEList[BaboonIssue], BaboonFamily]

  def reload(
    previous: Option[BaboonFamily],
    definitions: List[BaboonParser.ReloadInput],
  ): F[NEList[BaboonIssue], BaboonFamily]
}

object BaboonFamilyManager {
  class BaboonFamilyManagerImpl[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2](
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    comparator: BaboonComparator[F],
    logger: BLogger,
    validator: BaboonValidator[F],
    fileContentProvider: FileContentProvider,
  ) extends BaboonFamilyManager[F] {

    private case class DomainEntry(
      key: DomainKey,
      primaryFile: FSPath,
      files: Set[FSPath],
      deps: Set[DomainKey],
    )

    private case class BuildResult(
      family: BaboonFamily,
      domains: List[Domain],
    )

    override def load(
      definitions: List[BaboonParser.Input]
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      for {
        parsed            <- F.parTraverseAccumErrors(definitions)(parser.parse)
        parsedByPath       = definitions.zip(parsed).map { case (input, domain) => input.path -> domain }.toMap
        baseContentsByPath = definitions.map(input => input.path -> input.content).toMap
        domainIndex        = buildDomainIndex(parsed)
        contentsByPath     = expandContents(baseContentsByPath, domainIndex)
        buildResult <- buildFamily(
          parsed,
          BaboonIssue.of(TyperIssue.EmptyFamily(definitions)),
          Map.empty[DomainKey, Domain],
          domainIndex.keys,
        )
        cache = buildCache(contentsByPath, parsedByPath, domainIndex, buildResult.domains)
      } yield buildResult.family.copy(cache = cache)
    }

    override def reload(
      previous: Option[BaboonFamily],
      definitions: List[BaboonParser.ReloadInput],
    ): F[NEList[BaboonIssue], BaboonFamily] = {
      val snapshot = previous.map(_.cache).getOrElse(BaboonFamilyCache.empty)
      val (unparsed, parsed) = definitions.foldLeft((List.empty[BaboonParser.Input], List.empty[(FSPath, RawDomain)])) {
        case ((unparsedAcc, parsedAcc), input) =>
          input match {
            case BaboonParser.ReloadInput.Unparsed(path, content) =>
              (BaboonParser.Input(path, content) :: unparsedAcc, parsedAcc)
            case BaboonParser.ReloadInput.Parsed(path, content) =>
              (unparsedAcc, (path, content) :: parsedAcc)
          }
      }

      val unparsedInputs                  = unparsed.reverse
      val parsedInputs                    = parsed.reverse
      val parsedPaths                     = parsedInputs.map(_._1).toSet
      val inputContents                   = buildInputContents(snapshot, unparsedInputs, parsedInputs)
      val inputPaths                      = (unparsedInputs.map(_.path) ++ parsedInputs.map(_._1)).toSet
      val knownFiles                      = snapshot.domainFiles.values.flatten.toSet ++ inputPaths
      val (currentContents, missingFiles) = readKnownContents(inputContents, knownFiles)
      val removedFiles                    = snapshot.fileContents.keySet.diff(knownFiles) ++ missingFiles
      val newFiles                        = currentContents.keySet.diff(snapshot.fileContents.keySet)
      val modifiedFiles = currentContents.collect {
        case (path, content) if snapshot.fileContents.get(path).exists(_ != content) => path
      }.toSet
      val changedFiles         = removedFiles ++ newFiles ++ modifiedFiles ++ parsedPaths
      val affectedByFileChange = changedFiles.flatMap(path => snapshot.fileToDomains.getOrElse(path, Set.empty))
      val affectedPrimaryPaths = affectedByFileChange.flatMap(key => snapshot.domainPrimaryFiles.get(key))
      val toParse = unparsedInputs.filter {
        input =>
          val shouldParseForChange  = changedFiles.contains(input.path)
          val shouldParseForInclude = affectedPrimaryPaths.contains(input.path)
          val shouldParseForMissing = !snapshot.parsedByPath.contains(input.path)
          shouldParseForChange || shouldParseForInclude || shouldParseForMissing
      }
      val toParsePaths = toParse.map(_.path).toSet
      val retained = snapshot.parsedByPath.filter {
        case (path, _) =>
          inputPaths.contains(path) && !toParsePaths.contains(path) && !parsedPaths.contains(path)
      }

      val updatedContents = currentContents

      for {
        newlyParsed         <- F.parTraverseAccumErrors(toParse)(parser.parse)
        parsedFromInputs     = toParse.zip(newlyParsed).map { case (input, domain) => input.path -> domain }.toMap
        parsedProvided       = parsedInputs.toMap
        combinedByPath       = retained ++ parsedProvided ++ parsedFromInputs
        parsedList           = combinedByPath.values.toList
        domainIndex          = buildDomainIndex(parsedList)
        newKeys              = domainIndex.keys
        removedKeys          = snapshot.domainPrimaryFiles.keySet.diff(newKeys)
        addedKeys            = newKeys.diff(snapshot.domainPrimaryFiles.keySet)
        directlyAffectedKeys = affectedByFileChange ++ addedKeys ++ removedKeys
        affectedKeys = expandAffectedKeys(
          directlyAffectedKeys,
          removedKeys,
          domainIndex.depsByKey,
          snapshot.depsByKey,
          newKeys,
        )
        versionAffectedKeys = expandVersionAffected(
          affectedKeys ++ removedKeys,
          newKeys,
        )
        retypeKeys = (affectedKeys ++ versionAffectedKeys) ++ newKeys.diff(snapshot.typedByKey.keySet)
        _ <- F.maybeSuspend {
          val parsedCount      = parsedFromInputs.size
          val reusedCount      = retained.size + parsedProvided.size
          val uniqueDomainKeys = parsedList.map(keyOf).distinct
          val typedCount = uniqueDomainKeys.count {
            key =>
              retypeKeys.contains(key) || !snapshot.typedByKey.contains(key)
          }
          val domainReusedCount = uniqueDomainKeys.size - typedCount
          logger.message(
            "reload",
            s"files parsed=$parsedCount, reused=$reusedCount; domains typed=$typedCount, reused=$domainReusedCount",
          )
        }
        buildResult <- buildFamily(
          parsedList,
          BaboonIssue.of(TyperIssue.EmptyFamilyReload(definitions)),
          snapshot.typedByKey,
          retypeKeys,
        )
        cache = buildCache(updatedContents, combinedByPath, domainIndex, buildResult.domains)
      } yield buildResult.family.copy(cache = cache)
    }

    private def expandContents(
      baseContents: Map[FSPath, String],
      domainIndex: DomainIndex,
    ): Map[FSPath, String] = {
      val knownFiles = domainIndex.domainFiles.values.flatten.toSet
      val missing    = knownFiles.diff(baseContents.keySet)
      if (missing.isEmpty) {
        baseContents
      } else {
        val loaded = missing.map {
          path =>
            val content = fileContentProvider.read(path).getOrElse {
              throw new IllegalStateException(s"Missing content for $path")
            }
            path -> content
        }.toMap
        baseContents ++ loaded
      }
    }

    private def buildInputContents(
      snapshot: BaboonFamilyCache,
      unparsedInputs: List[BaboonParser.Input],
      parsedInputs: List[(FSPath, RawDomain)],
    ): Map[FSPath, String] = {
      val contents = mutable.Map.empty[FSPath, String]
      unparsedInputs.foreach {
        input =>
          contents.put(input.path, input.content)
      }
      val missingParsed = mutable.ListBuffer.empty[FSPath]
      parsedInputs.foreach {
        case (path, _) =>
          snapshot.fileContents.get(path) match {
            case Some(content) => contents.put(path, content)
            case None          => missingParsed += path
          }
      }
      if (missingParsed.nonEmpty) {
        throw new IllegalStateException(s"Missing cached contents for parsed inputs: ${missingParsed.mkString(", ")}")
      }
      contents.toMap
    }

    private def readKnownContents(
      inputContents: Map[FSPath, String],
      knownFiles: Set[FSPath],
    ): (Map[FSPath, String], Set[FSPath]) = {
      val contents = mutable.Map.empty[FSPath, String]
      contents ++= inputContents
      val missing = mutable.Set.empty[FSPath]
      knownFiles.foreach {
        path =>
          if (!contents.contains(path)) {
            fileContentProvider.read(path) match {
              case Some(content) => contents.put(path, content)
              case None          => missing.add(path)
            }
          }
      }
      (contents.toMap, missing.toSet)
    }

    private def buildFamily(
      parsed: List[RawDomain],
      emptyFamilyIssue: NEList[BaboonIssue],
      cachedTyped: Map[DomainKey, Domain],
      retypeKeys: Set[DomainKey],
    ): F[NEList[BaboonIssue], BuildResult] = {
      for {
        resolvedImports <- resolveImports(parsed)
        domains <- F.parTraverseAccumErrors(resolvedImports) {
          raw =>
            val key = keyOf(raw)
            cachedTyped.get(key) match {
              case Some(cached) if !retypeKeys.contains(key) =>
                F.pure(cached)
              case _ =>
                typer.process(raw)
            }
        }
        _ <- F.maybeSuspend {
          domains.sortBy(d => (d.id.toString, d.version)).foreach {
            d =>
              logger.message(
                d.id.toString,
                q"${d.version}: retained definitions: ${d.defs.meta.nodes.size}, unreachable definitions: ${d.excludedIds.size}",
              )
          }
        }

        lineages <- F.parTraverseAccumErrors(
          domains
            .map(d => (d.id, d))
            .toMultimap
            .toSeq
        ) {
          case (pkg, domains) =>
            for {
              uniqueVersions <- F.fromEither {
                domains
                  .map(d => (d.version, d))
                  .toUniqueMap(v => BaboonIssue.of(TyperIssue.NonUniqueDomainVersions(v)))
              }
              nel <- F.fromOption(BaboonIssue.of(TyperIssue.EmptyDomainFamily(pkg))) {
                NEMap.from(uniqueVersions)
              }
              evo <- comparator.evolve(pkg, nel)
            } yield {
              BaboonLineage(pkg, nel, evo)
            }
        }

        uniqueLineages <- F.fromEither {
          lineages
            .map(l => (l.pkg, l))
            .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueLineages(e)))
        }

        nem <- F.fromOption(emptyFamilyIssue) {
          NEMap.from(uniqueLineages)
        }
        fam = BaboonFamily(nem, BaboonFamilyCache.empty)
        _  <- validator.validate(fam)

      } yield {
        BuildResult(fam, domains)
      }
    }

    def toposorted[N, M](g: DAG[N, M]): Seq[N] = {
      val roots = g.noPredcessors.toSeq
      def go(out: Seq[N]): Seq[N] = {
        out ++ out.flatMap(n => go(g.successors.links(n).toSeq))
      }
      go(roots)
    }

    def withImports(id: DomainKey, idx: collection.Map[DomainKey, RawDomain]): RawDomain = {
      val current = idx(id)
      assert(current.members.includes.isEmpty)

      val importedMembers = current.imported
        .foldLeft(List.empty[(RawTypeName, RawTLDef)]) {
          case (acc, v) =>
            val importedDom = idx(DomainKey(id.id, v.value))

            val imported = importedDom.members.defs

            val recursivelyImported = importedDom.imported.toSeq.flatMap(i => withImports(DomainKey(id.id, i.value), idx).members.defs)

            val allImported = (imported ++ recursivelyImported).map(d => (d.value.name, d)).filterNot { case (n, _) => v.without.contains(n) }

            acc ++ allImported
        }.toMultimap

      val currentMembers = current.members.defs.map(d => (d.value.name, d)).toMultimap

      val fullMembers = (importedMembers ++ currentMembers).unwrap.map(_._2)

      RawDomain(current.header, current.version, current.pragmas, None, RawContent(Seq.empty, fullMembers))
    }

    private def resolveImports(parsed: List[RawDomain]): F[NEList[BaboonIssue], List[RawDomain]] = {
      import izumi.fundamentals.collections.IzCollections.*

      for {
        indexed <- F.fromEither(
          parsed
            .map(d => (DomainKey(d.header.name.mkString("."), d.version.value), d))
            .toUniqueMap(e => BaboonIssue.of(TyperIssue.NonUniqueRawDomainVersion(e)))
        )
        deps   = indexed.view.mapValues(d => d.imported.map(i => DomainKey(d.header.name.mkString("."), i.value)).toSet).toMap
        graph <- buildDomainDag(indexed, deps, parsed.head.header.meta)
        sorted = toposorted(graph)
      } yield {

        val wip = mutable.HashMap.from(indexed)

        sorted.foreach {
          id =>
            wip.put(id, withImports(id, wip))
        }

        wip.values.toList
      }

    }

    private case class DomainIndex(
      domainPrimaryFiles: Map[DomainKey, FSPath],
      domainFiles: Map[DomainKey, Set[FSPath]],
      fileToDomains: Map[FSPath, Set[DomainKey]],
      depsByKey: Map[DomainKey, Set[DomainKey]],
      keys: Set[DomainKey],
    )

    private def buildDomainIndex(domains: List[RawDomain]): DomainIndex = {
      val entries = domains.map(domainEntry)
      val primaryFiles = entries.groupBy(_.key).map {
        case (key, grouped) =>
          key -> grouped.head.primaryFile
      }
      val domainFiles = entries.groupBy(_.key).map {
        case (key, grouped) =>
          key -> grouped.flatMap(_.files).toSet
      }
      val depsByKey = entries.groupBy(_.key).map {
        case (key, grouped) =>
          key -> grouped.flatMap(_.deps).toSet
      }
      val fileToDomains = domainFiles.toSeq.flatMap {
        case (key, files) =>
          files.map(file => file -> key)
      }
        .groupBy(_._1)
        .map {
          case (file, pairs) =>
            file -> pairs.map(_._2).toSet
        }
      DomainIndex(primaryFiles, domainFiles, fileToDomains, depsByKey, primaryFiles.keySet)
    }

    private def domainEntry(domain: RawDomain): DomainEntry = {
      val key     = keyOf(domain)
      val primary = requirePrimaryFile(domain.header.meta)
      val files   = collectDomainFiles(domain) + primary
      val deps = domain.imported match {
        case Some(imported) =>
          Set(DomainKey(key.id, imported.value))
        case None =>
          Set.empty[DomainKey]
      }
      DomainEntry(key, primary, files, deps)
    }

    private def keyOf(domain: RawDomain): DomainKey = {
      DomainKey(domain.header.name.mkString("."), domain.version.value)
    }

    private def keyOf(domain: Domain): DomainKey = {
      DomainKey(domain.id.toString, domain.version.toString)
    }

    private def requirePrimaryFile(meta: RawNodeMeta): FSPath = {
      meta.pos match {
        case fk: io.septimalmind.baboon.parser.model.InputPointer.FileKnown =>
          fk.file
        case _ =>
          throw new IllegalStateException("Domain header has no file information")
      }
    }

    private def collectDomainFiles(domain: RawDomain): Set[FSPath] = {
      val headerFiles  = filesFromMeta(domain.header.meta)
      val versionFiles = filesFromMeta(domain.version.meta)
      val importFiles = domain.imported match {
        case Some(imported) => filesFromMeta(imported.meta)
        case None           => Set.empty
      }
      val defFiles = domain.members.defs.flatMap(collectFiles).toSet
      headerFiles ++ versionFiles ++ importFiles ++ defFiles
    }

    private def filesFromMeta(meta: RawNodeMeta): Set[FSPath] = {
      meta.pos match {
        case fk: io.septimalmind.baboon.parser.model.InputPointer.FileKnown =>
          Set(fk.file)
        case _ =>
          Set.empty
      }
    }

    private def collectFiles(defn: RawTLDef): Set[FSPath] = {
      defn match {
        case RawTLDef.Enum(_, value) =>
          collectFilesFromDefn(value)
        case RawTLDef.DTO(_, value) =>
          collectFilesFromDefn(value)
        case RawTLDef.ADT(_, value) =>
          collectFilesFromDefn(value)
        case RawTLDef.Foreign(_, value) =>
          collectFilesFromDefn(value)
        case RawTLDef.Contract(_, value) =>
          collectFilesFromDefn(value)
        case RawTLDef.Service(_, value) =>
          collectFilesFromDefn(value)
        case RawTLDef.Namespace(value) =>
          collectFilesFromDefn(value)
      }
    }

    private def filesFromDtoMember(member: io.septimalmind.baboon.parser.model.RawDtoMember): Set[FSPath] = {
      member match {
        case io.septimalmind.baboon.parser.model.RawDtoMember.FieldDef(_, meta) =>
          filesFromMeta(meta)
        case io.septimalmind.baboon.parser.model.RawDtoMember.UnfieldDef(_, meta) =>
          filesFromMeta(meta)
        case io.septimalmind.baboon.parser.model.RawDtoMember.ParentDef(_, meta) =>
          filesFromMeta(meta)
        case io.septimalmind.baboon.parser.model.RawDtoMember.UnparentDef(_, meta) =>
          filesFromMeta(meta)
        case io.septimalmind.baboon.parser.model.RawDtoMember.IntersectionDef(_, meta) =>
          filesFromMeta(meta)
        case io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef(_, meta) =>
          filesFromMeta(meta)
      }
    }

    private def filesFromFunc(func: io.septimalmind.baboon.parser.model.RawFunc): Set[FSPath] = {
      val argFiles = func.sig.flatMap(filesFromFuncArg).toSet
      filesFromMeta(func.meta) ++ argFiles
    }

    private def filesFromFuncArg(arg: io.septimalmind.baboon.parser.model.RawFuncArg): Set[FSPath] = {
      arg match {
        case io.septimalmind.baboon.parser.model.RawFuncArg.Ref(_, _, meta) =>
          filesFromMeta(meta)
        case io.septimalmind.baboon.parser.model.RawFuncArg.Struct(defn) =>
          collectFilesFromDefn(defn)
      }
    }

    private def collectFilesFromDefn(defn: io.septimalmind.baboon.parser.model.RawDefn): Set[FSPath] = {
      defn match {
        case dto: io.septimalmind.baboon.parser.model.RawDto =>
          filesFromMeta(dto.meta) ++ dto.members.flatMap(filesFromDtoMember).toSet
        case contract: io.septimalmind.baboon.parser.model.RawContract =>
          filesFromMeta(contract.meta) ++ contract.members.flatMap(filesFromDtoMember).toSet
        case enum: io.septimalmind.baboon.parser.model.RawEnum =>
          filesFromMeta(enum.meta) ++ enum.members.flatMap(m => filesFromMeta(m.meta)).toSet
        case adt: io.septimalmind.baboon.parser.model.RawAdt =>
          val contractFiles = adt.contracts.flatMap(c => filesFromMeta(c.meta)).toSet
          filesFromMeta(adt.meta) ++ contractFiles ++ adt.members.flatMap {
            member =>
              filesFromMeta(member.meta) ++ collectFilesFromDefn(member.defn)
          }.toSet
        case foreign: io.septimalmind.baboon.parser.model.RawForeign =>
          filesFromMeta(foreign.meta)
        case namespace: io.septimalmind.baboon.parser.model.RawNamespace =>
          filesFromMeta(namespace.meta) ++ namespace.defns.flatMap(collectFiles).toSet
        case service: io.septimalmind.baboon.parser.model.RawService =>
          filesFromMeta(service.meta) ++ service.defns.flatMap(filesFromFunc).toSet
      }
    }

    private def expandAffectedKeys(
      directlyAffected: Set[DomainKey],
      removedKeys: Set[DomainKey],
      depsByKey: Map[DomainKey, Set[DomainKey]],
      previousDepsByKey: Map[DomainKey, Set[DomainKey]],
      currentKeys: Set[DomainKey],
    ): Set[DomainKey] = {
      val affectedFromCurrent = collectDependents(depsByKey, directlyAffected.intersect(currentKeys))
      val affectedFromRemoved = collectDependents(previousDepsByKey, removedKeys)
      directlyAffected ++ affectedFromCurrent ++ affectedFromRemoved
    }

    private def expandVersionAffected(
      affectedKeys: Set[DomainKey],
      currentKeys: Set[DomainKey],
    ): Set[DomainKey] = {
      val grouped = currentKeys.groupBy(_.id)
      affectedKeys.flatMap {
        key =>
          val baseVersion = io.septimalmind.baboon.typer.model.Version.parse(key.version)
          grouped.getOrElse(key.id, Set.empty).filter {
            candidate =>
              val candidateVersion = io.septimalmind.baboon.typer.model.Version.parse(candidate.version)
              candidateVersion >= baseVersion
          }
      }
    }

    private def collectDependents(
      depsByKey: Map[DomainKey, Set[DomainKey]],
      start: Set[DomainKey],
    ): Set[DomainKey] = {
      if (start.isEmpty) {
        return Set.empty
      }
      val graph = buildDependencyDag(depsByKey ++ start.map(_ -> Set.empty[DomainKey]).toMap)

      @annotation.tailrec
      def loop(frontier: Set[DomainKey], acc: Set[DomainKey]): Set[DomainKey] = {
        if (frontier.isEmpty) {
          acc
        } else {
          val next = frontier.flatMap(key => graph.successors.links(key).toSet).diff(acc)
          loop(next, acc ++ next)
        }
      }
      loop(start, start)
    }

    private def buildCache(
      contentsByPath: Map[FSPath, String],
      parsedByPath: Map[FSPath, RawDomain],
      domainIndex: DomainIndex,
      domains: List[Domain],
    ): BaboonFamilyCache = {
      BaboonFamilyCache(
        contentsByPath,
        parsedByPath,
        domains.map(domain => keyOf(domain) -> domain).toMap,
        domainIndex.domainFiles,
        domainIndex.domainPrimaryFiles,
        domainIndex.fileToDomains,
        domainIndex.depsByKey,
      )
    }

    private def buildDomainDag(
      indexed: Map[DomainKey, RawDomain],
      deps: Map[DomainKey, Set[DomainKey]],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], DAG[DomainKey, RawDomain]] = {
      val graphNodes = GraphMeta(indexed)
      val preds      = AdjacencyPredList(deps)
      // TODO: BUG in fromPred, it same as fromSucc but should be transposed
      F.fromEither(DAG.fromPred(preds, graphNodes).left.map(e => BaboonIssue.of(TyperIssue.DagError(e, meta))))
    }

    private def buildDependencyDag(
      deps: Map[DomainKey, Set[DomainKey]]
    ): DAG[DomainKey, DomainKey] = {
      val nodes = GraphMeta((deps.keySet ++ deps.valuesIterator.flatten.toSet).map(k => k -> k).toMap)
      val preds = AdjacencyPredList(deps)
      DAG
        .fromPred(preds, nodes)
        .fold(e => throw new IllegalStateException(s"Invalid dependency graph: $e"), identity)
    }

  }
}
