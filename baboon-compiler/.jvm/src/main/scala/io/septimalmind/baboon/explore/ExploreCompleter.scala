package io.septimalmind.baboon.explore

import io.septimalmind.baboon.explore.commands.CommandRegistry
import org.jline.reader.{Candidate, Completer, LineReader, ParsedLine}

import java.util.{List => JList}

class ExploreCompleter(registry: CommandRegistry) extends Completer {
  override def complete(reader: LineReader, line: ParsedLine, candidates: JList[Candidate]): Unit = {
    val buffer = line.line()
    val cursor = line.cursor()

    val completions = registry.completeCommand(buffer, cursor)

    completions.foreach {
      completion =>
        candidates.add(new Candidate(completion))
    }
  }
}
