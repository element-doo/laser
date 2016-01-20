package services

import scala.util.Try
import scalax.file.Path
import scalax.io.StandardOpenOption

object Store {

  def path: Path = Path("") / "var" / "www" / "rules.txt"
  def storeRules(rules: String): Try[String] = Try {
    //path.write(rules)
    path.outputStream(StandardOpenOption.Write).write(rules)
    rules
  }

  def readRules: Try[String] = Try {
    path.inputStream.string
  }
}