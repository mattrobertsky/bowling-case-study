package code

import org.scalatest._

class MainSpec extends WordSpec with Matchers {
  "A game with scores X X X X X X X X X X X X" should {
    "return a total score of 300" in {
      Main.calculateScore("X X X X X X X X X X X X") shouldBe 300
    }
  }
}
