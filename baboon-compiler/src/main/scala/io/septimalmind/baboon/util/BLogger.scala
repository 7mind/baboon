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
    override def message(msg: String): Unit = ()
    override def message(context: String, msg: String): Unit = ()
    override def message(msg: TextTree[Any]): Unit = ()
    override def message(context: String, msg: TextTree[Any]): Unit = ()
  }

  class BLoggerImpl extends BLogger {
    override def message(msg: String): Unit = {
      doMessage(msg)
    }

    override def message(context: String, msg: String): Unit = {
      if (IzPlatform.terminalColorsEnabled) {
        doMessage(s"${AnsiColor.MAGENTA}[ $context ]${AnsiColor.RESET} $msg")
      } else {
        doMessage(s"[ $context ] $msg")
      }
    }

    override def message(msg: TextTree[Any]): Unit = {
      doMessage(renderTree(msg))
    }

    private def renderTree(msg: TextTree[Any]): String = {
      msg.mapRender {
        v =>
          if (IzPlatform.terminalColorsEnabled) {
            s"${AnsiColor.GREEN}$v${AnsiColor.RESET}"
          } else {
            v.toString
          }
      }
    }

    private def doMessage(msg: String): Unit = {
      val uptime = ManagementFactory.getRuntimeMXBean.getUptime
      val d      = FiniteDuration(uptime, TimeUnit.MILLISECONDS)
      val cd = if (IzPlatform.terminalColorsEnabled) {
        s"${AnsiColor.CYAN}${d.toMillis}ms${AnsiColor.RESET}"
      } else {
        d
      }

      Console.println(s"$cd: $msg")
    }

    override def message(context: String, msg: TextTree[Any]): Unit = {
      message(context, renderTree(msg))
    }
  }
}
