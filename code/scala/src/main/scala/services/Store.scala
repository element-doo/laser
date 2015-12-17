package services

import scala.util.Try
import scalax.file.Path

object Store {

  def path: Path = Path("") / "var" / "www" / "rules.txt"
  def storeRules(rules: String): Try[String] = Try {
    path.write(rules)
    rules
  }
  def readRules: Try[String] = Try {
    path.inputStream().string
  }
}
