package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile}
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

case class OutputFileWithPath(path: String, content: String, product: CompilerProduct)

trait BaboonCompilerJS[F[+_, +_]] {
  def run(target: CompilerTargetJS, model: BaboonFamily): F[NEList[BaboonIssue], Seq[OutputFileWithPath]]
}

class BaboonCompilerJSImpl[F[+_, +_]: Error2](
  translator: BaboonAbstractTranslator[F],
) extends BaboonCompilerJS[F] {

  override def run(target: CompilerTargetJS, model: BaboonFamily): F[NEList[BaboonIssue], Seq[OutputFileWithPath]] = {
    for {
      sources <- translator.translate(model)
    } yield {
      // Filter files based on what products are enabled and convert to OutputFileWithPath
      sources.files
        .filter { case (_, file) => target.output.products.contains(file.product) }
        .map { case (path, file) => OutputFileWithPath(path, file.content, file.product) }
        .toSeq
    }
  }
}
