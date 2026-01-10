package io.septimalmind.baboon.lsp.state

import java.net.URI
import scala.collection.concurrent.TrieMap

class DocumentState {
  private val documents = TrieMap.empty[String, DocumentInfo]

  case class DocumentInfo(
    uri: String,
    content: String,
    version: Int
  )

  def open(uri: String, content: String): Unit = {
    documents.put(uri, DocumentInfo(uri, content, 0))
  }

  def update(uri: String, content: String): Unit = {
    documents.updateWith(uri) {
      case Some(info) => Some(info.copy(content = content, version = info.version + 1))
      case None       => Some(DocumentInfo(uri, content, 0))
    }
  }

  def close(uri: String): Unit = {
    documents.remove(uri)
  }

  def getContent(uri: String): Option[String] = {
    documents.get(uri).map(_.content)
  }

  def getOpenDocuments: Seq[String] = {
    documents.keys.toSeq
  }

  def getAllDocuments: Map[String, String] = {
    documents.view.mapValues(_.content).toMap
  }

  def uriToPath(uri: String): String = {
    new URI(uri).getPath
  }

  def pathToUri(path: String): String = {
    new java.io.File(path).toURI.toString
  }
}
