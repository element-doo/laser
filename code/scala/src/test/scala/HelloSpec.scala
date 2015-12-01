import org.scalatest._

class LauncherSpec extends FlatSpec with Matchers {
  "Launcher" should "have tests" in {
    true should === (true)
  }
}
