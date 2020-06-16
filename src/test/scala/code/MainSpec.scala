package code

import org.scalatest._

class MainSpec extends WordSpec with Matchers {

  "A game with scores X X X X X X X X X X X X" should {
    "return a total score of 300" in {
      Main.calculateScore("X X X X X X X X X X X X") shouldBe 300
    }
  }

  "A game with scores 9- 9- 9- 9- 9- 9- 9- 9- 9- 9-" should {
    "return a total score of 90" in {
      Main.calculateScore("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-") shouldBe 90
    }
  }

  "A game with scores 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5" should {
    "return a total score of 150" in {
      Main.calculateScore("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5") shouldBe 150
    }
  }

  "A game with scores 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5" should {
    "return a total score of 150" in {
      Main.calculateScore("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5") shouldBe 150
    }
  }

  "A game with scores X X X X X X X X X X 12 --" should {
    "return a total score of 274" in {
      Main.calculateScore("X X X X X X X X X X 12 --") shouldBe 274
    }
  }

}
