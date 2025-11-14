package izumi.fundamentals.platform.resources

import scala.util.Try

object IzResources {
  def readAsString(@annotation.unused path: String): Try[String] = {
    // In JS environment, resources should be bundled as strings at compile time
    // For now, return empty to allow compilation
    Try("")
  }
}
