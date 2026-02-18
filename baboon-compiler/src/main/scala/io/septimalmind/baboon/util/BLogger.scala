package io.septimalmind.baboon.util

import izumi.fundamentals.platform.IzPlatform
import izumi.fundamentals.platform.strings.TextTree

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.io.AnsiColor

trait BLogger {
  def message(context: String, msg: TextTree[Any]): Unit
  def message(msg: TextTree[Any]): Unit

  def message(msg: String): Unit
  def message(context: String, msg: String): Unit
}

object BLogger {
  object Noop extends BLogger {
    override def message(msg: String): Unit                         = ()
    override def message(context: String, msg: String): Unit        = ()
    override def message(msg: TextTree[Any]): Unit                  = ()
    override def message(context: String, msg: TextTree[Any]): Unit = ()
  }

  abstract class BaseLogger extends BLogger {
    override def message(msg: String): Unit = {
      doMessage(msg)
    }

    override def message(context: String, msg: String): Unit = {
      if (colorsEnabled) {
        doMessage(s"${AnsiColor.MAGENTA}[ $context ]${AnsiColor.RESET} $msg")
      } else {
        doMessage(s"[ $context ] $msg")
      }
    }

    override def message(msg: TextTree[Any]): Unit = {
      doMessage(renderTree(msg))
    }

    override def message(context: String, msg: TextTree[Any]): Unit = {
      message(context, renderTree(msg))
    }

    protected def writeLine(msg: String): Unit
    protected def colorsEnabled: Boolean = IzPlatform.terminalColorsEnabled

    private def renderTree(msg: TextTree[Any]): String = {
      msg.mapRender {
        v =>
          if (colorsEnabled) {
            s"${AnsiColor.GREEN}$v${AnsiColor.RESET}"
          } else {
            v.toString
          }
      }
    }

    private def doMessage(msg: String): Unit = {
      val uptime = ManagementFactory.getRuntimeMXBean.getUptime
      val d      = FiniteDuration(uptime, TimeUnit.MILLISECONDS)
      val cd = if (colorsEnabled) {
        s"${AnsiColor.CYAN}${d.toMillis}ms${AnsiColor.RESET}"
      } else {
        d
      }

      writeLine(s"$cd: $msg")
    }
  }

  final class BLoggerImpl extends BaseLogger {
    override protected def writeLine(msg: String): Unit = {
      Console.println(msg)
    }
  }

  final class BLoggerErrImpl extends BaseLogger {
    override protected def colorsEnabled: Boolean = false

    override protected def writeLine(msg: String): Unit = {
      System.err.println(msg)
    }
  }
}
