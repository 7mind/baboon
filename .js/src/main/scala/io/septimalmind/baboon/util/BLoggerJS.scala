package io.septimalmind.baboon.util

import izumi.fundamentals.platform.strings.TextTree

class BLoggerJS(debug: Boolean) extends BLogger {
  override def message(msg: String): Unit = {
    if (debug) {
      println(msg)
    }
  }

  override def message(context: String, msg: String): Unit = {
    if (debug) {
      println(s"[$context] $msg")
    }
  }

  override def message(msg: TextTree[Any]): Unit = {
    if (debug) {
      println(renderTree(msg))
    }
  }

  override def message(context: String, msg: TextTree[Any]): Unit = {
    if (debug) {
      println(s"[$context] ${renderTree(msg)}")
    }
  }

  private def renderTree(msg: TextTree[Any]): String = {
    msg.mapRender(_.toString)
  }
}
