package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{EitherF, ExploreContext}

trait Command {
  def name: String
  def aliases: Seq[String] = Seq.empty
  def help: String
  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String]
  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String]
}

class CommandRegistry(ctx: ExploreContext[EitherF]) {
  private val commandList: Seq[Command] = Seq(
    DomainsCommand,
    VersionsCommand,
    SwitchCommand,
    TypesCommand,
    ShowCommand,
    ExampleCommand,
    DecodeCommand,
    DepsCommand,
    DepsOfCommand,
    DeptreeCommand,
    EvoCommand,
    HelpCommand,
  )

  private val commands: Map[String, Command] =
    commandList.flatMap(cmd => (cmd.name +: cmd.aliases).map(_ -> cmd)).toMap

  def all: Seq[Command] = commandList

  def get(name: String): Option[Command] = commands.get(name.toLowerCase)

  def commandNames: Seq[String] = commandList.flatMap(c => c.name +: c.aliases).sorted

  def dispatch(line: String): Either[String, String] = {
    val parts = parseLine(line)
    parts.headOption match {
      case None => Right("")
      case Some(cmdName) =>
        commands.get(cmdName.toLowerCase) match {
          case Some(cmd) => cmd.execute(parts.tail, ctx)
          case None => Left(s"Unknown command: $cmdName. Type 'help' for available commands.")
        }
    }
  }

  def completeCommand(buffer: String, cursor: Int): Seq[String] = {
    val parts = parseLine(buffer.take(cursor))
    val currentWord = if (buffer.endsWith(" ")) "" else parts.lastOption.getOrElse("")

    if (parts.length <= 1 && !buffer.endsWith(" ")) {
      commandNames.filter(_.startsWith(currentWord.toLowerCase))
    } else {
      val cmdName = parts.headOption.getOrElse("")
      commands.get(cmdName.toLowerCase) match {
        case Some(cmd) =>
          val cmdArgs = if (buffer.endsWith(" ")) parts.tail :+ "" else parts.tail
          cmd.complete(cmdArgs, ctx)
        case None =>
          Seq.empty
      }
    }
  }

  private def parseLine(line: String): Seq[String] = {
    line.trim.split("\\s+").filter(_.nonEmpty).toSeq
  }
}
