package io.septimalmind.baboon.parser

import io.septimalmind.baboon.parser.model.{FSPath, RawInclude}
import io.septimalmind.baboon.util.{StoredZipReader, Utf8, ZipArchiveEntry}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}

/** The parser inputs of a schema archive (the `loadMany` contract, docs/cli-reference.md `:scheme`):
  * every `*.baboon` entry is a model; `*.bmo` entries exist only to be included; directory entries
  * are ignored; any other entry is an error. Entry paths are kept verbatim, so diagnostics name the
  * archive path, and `include` paths resolve against the archive root — the role a `--model-dir`
  * plays on the command line.
  */
final case class BaboonArchiveInputs(models: NEList[BaboonParser.Input], includables: List[BaboonParser.Input]) {
  def all: List[BaboonParser.Input] = models.toList ++ includables
}

object BaboonArchiveInputs {
  private val ModelExtension   = ".baboon"
  private val IncludeExtension = ".bmo"
  private val Separator        = "/"
  private val DrivePrefix      = "^[A-Za-z]:".r

  def fromZip(bytes: Array[Byte]): Either[NEList[String], BaboonArchiveInputs] = {
    StoredZipReader.read(bytes).left.map(NEList(_)).flatMap(fromEntries)
  }

  def fromEntries(entries: List[ZipArchiveEntry]): Either[NEList[String], BaboonArchiveInputs] = {
    val pathErrors = entries.flatMap(entry => validatePath(entry.path).map(error => s"unsafe archive entry path '${entry.path}': $error"))
    val duplicates = entries
      .groupBy(_.path.stripSuffix(Separator)).collect {
        case (path, sameName) if sameName.size > 1 => s"duplicate archive entry path '$path'"
      }.toList.sorted
    NEList.from(pathErrors ++ duplicates) match {
      case Some(errors) => Left(errors)
      case None         => classify(entries.filterNot(_.isDirectory))
    }
  }

  // paths are validated first: FSPath.parse assumes non-empty segments
  private def classify(files: List[ZipArchiveEntry]): Either[NEList[String], BaboonArchiveInputs] = {
    val classified = files.map {
      entry =>
        if (entry.path.endsWith(ModelExtension) || entry.path.endsWith(IncludeExtension)) {
          Utf8
            .decodeStrict(entry.data)
            .left.map(_ => s"archive entry '${entry.path}' is not valid UTF-8")
            .map(content => (entry.path.endsWith(ModelExtension), BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(entry.path)), content)))
        } else {
          Left(s"unsupported archive entry '${entry.path}': only *$ModelExtension schemas, *$IncludeExtension includes and directories are allowed")
        }
    }
    NEList.from(classified.collect { case Left(error) => error }) match {
      case Some(errors) => Left(errors)
      case None =>
        val inputs = classified.collect { case Right(input) => input }
        NEList.from(inputs.collect { case (true, input) => input }) match {
          case None         => Left(NEList(s"the archive contains no *$ModelExtension schema"))
          case Some(models) => Right(BaboonArchiveInputs(models, inputs.collect { case (false, input) => input }))
        }
    }
  }

  private def validatePath(path: String): Option[String] = {
    val segments = path.stripSuffix(Separator).split(Separator, -1).toList
    if (path.isEmpty || path == Separator) Some("empty name")
    else if (path.startsWith(Separator)) Some("absolute path")
    else if (path.contains("\\")) Some("backslash separator")
    else if (path.contains("\u0000")) Some("NUL character")
    else if (DrivePrefix.findPrefixOf(path).nonEmpty) Some("drive prefix")
    else if (segments.exists(segment => segment.isEmpty || segment == "." || segment == "..")) Some("empty, '.' or '..' segment")
    else None
  }

  /** Resolves `include` paths against the archive root, as `--model-dir` does for directories. */
  final class ArchiveInclusionResolver[F[+_, +_]](inputs: Seq[BaboonParser.Input]) extends BaboonInclusionResolver[F] {
    private val byPath: Map[String, BaboonParser.Input] = inputs.map(input => input.path.asString -> input).toMap

    override def resolveInclude(inc: RawInclude): Option[(FSPath, String)] = {
      normalize(inc.value).flatMap(byPath.get).map(input => input.path -> input.content)
    }

    // a path leaving the archive root names nothing in it
    private def normalize(path: String): Option[String] = {
      if (path.startsWith(Separator)) {
        None
      } else {
        path
          .split(Separator).toList.filterNot(segment => segment.isEmpty || segment == ".")
          .foldLeft(Option(List.empty[String])) {
            case (Some(_ :: parent), "..") => Some(parent)
            case (Some(Nil), "..")         => None
            case (Some(stack), segment)    => Some(segment :: stack)
            case (None, _)                 => None
          }
          .map(_.reverse.mkString(Separator))
      }
    }
  }
}
