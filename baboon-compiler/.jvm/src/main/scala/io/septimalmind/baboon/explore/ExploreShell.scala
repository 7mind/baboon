package io.septimalmind.baboon.explore

import io.septimalmind.baboon.explore.commands.CommandRegistry
import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}
import org.jline.reader.LineReader
import org.jline.terminal.TerminalBuilder

class ExploreShell(ctx: ExploreContext) {
  private val terminal = TerminalBuilder.builder()
    .system(true)
    .build()

  private val registry = new CommandRegistry(ctx)
  private val completer = new ExploreCompleter(registry)

  private val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .completer(completer)
    .option(LineReader.Option.CASE_INSENSITIVE, true)
    .option(LineReader.Option.AUTO_LIST, true)
    .option(LineReader.Option.AUTO_MENU, true)
    .option(LineReader.Option.LIST_AMBIGUOUS, true)
    .build()

  def run(): Unit = {
    printWelcome()

    var running = true
    while (running) {
      try {
        val line = reader.readLine(buildPrompt())
        if (line == null) {
          running = false
        } else {
          val trimmed = line.trim
          if (trimmed == "exit" || trimmed == "quit") {
            running = false
          } else if (trimmed.nonEmpty) {
            registry.dispatch(trimmed) match {
              case Right(output) if output.nonEmpty =>
                terminal.writer().println(output)
                terminal.writer().println()
              case Right(_) =>
              case Left(error) =>
                terminal.writer().println(s"${Colors.RED}Error: $error${Colors.RESET}")
                terminal.writer().println()
            }
            terminal.writer().flush()
          }
        }
      } catch {
        case _: UserInterruptException =>
        case _: EndOfFileException =>
          running = false
      }
    }

    terminal.writer().println("Goodbye!")
    terminal.close()
  }

  private def printWelcome(): Unit = {
    val domains = ctx.allDomains
    terminal.writer().println(s"${Colors.CYAN}Baboon Interactive Explorer${Colors.RESET}")
    terminal.writer().println(s"Loaded ${domains.size} domain(s)")
    terminal.writer().println(s"Type ${Colors.GREEN}help${Colors.RESET} for available commands, ${Colors.GREEN}exit${Colors.RESET} to quit")
    terminal.writer().println()

    domains match {
      case Seq(singleDomain) =>
        ctx.switchTo(singleDomain, None) match {
          case Right(_) =>
            val version = ctx.currentVersion.get
            terminal.writer().println(s"${Colors.GREEN}Auto-selected${Colors.RESET} ${singleDomain.path.mkString(".")} v:$version")
          case Left(err) =>
            terminal.writer().println(s"${Colors.RED}Failed to auto-select domain: $err${Colors.RESET}")
        }
        terminal.writer().println()

      case multiple if multiple.nonEmpty =>
        terminal.writer().println(s"${Colors.CYAN}Available domains:${Colors.RESET}")
        multiple.foreach { pkg =>
          val lineage = ctx.family.domains.toMap(pkg)
          val versionCount = lineage.versions.toMap.size
          terminal.writer().println(s"  ${pkg.path.mkString(".")} ${Colors.DIM}($versionCount version(s))${Colors.RESET}")
        }
        terminal.writer().println()
        terminal.writer().println(s"Use ${Colors.GREEN}switch <domain>${Colors.RESET} to select a domain")
        terminal.writer().println()

      case _ =>
        terminal.writer().println(s"${Colors.YELLOW}No domains loaded${Colors.RESET}")
        terminal.writer().println()
    }

    terminal.writer().flush()
  }

  private def buildPrompt(): String = {
    val pkgPart = ctx.currentPkg.map(_.path.mkString(".")).getOrElse("(no domain)")
    val verPart = ctx.currentVersion.map(v => s" v:$v").getOrElse("")
    s"${Colors.BLUE}baboon${Colors.RESET} $pkgPart$verPart ${Colors.YELLOW}#${Colors.RESET} "
  }
}

object Colors {
  val RESET = "\u001b[0m"
  val RED = "\u001b[31m"
  val GREEN = "\u001b[32m"
  val YELLOW = "\u001b[33m"
  val BLUE = "\u001b[34m"
  val MAGENTA = "\u001b[35m"
  val CYAN = "\u001b[36m"
  val WHITE = "\u001b[37m"
  val BOLD = "\u001b[1m"
  val DIM = "\u001b[2m"
}
