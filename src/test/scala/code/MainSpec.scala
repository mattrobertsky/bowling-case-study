package code

import org.scalatest._

class MainSpec extends WordSpec with Matchers {
  "A game with scores X X X X X X X X X X X X" should {
    "return a total score of 300" in {
      Main.calculateScore("X X X X X X X X X X X X") shouldBe 300
    }
  }

  "A strike frame X " should {
    "return a Frame(10,0) " in {
      Frame.build("X") shouldBe Frame(10,0)
    }
  }

  "A spare frame 5/ " should {
    "return a Frame(5,5) " in {
      Frame.build("5/") shouldBe Frame(5,5)
    }
  }

  "A miss frame 5- " should {
    "return a Frame(5,0) " in {
      Frame.build("5-") shouldBe Frame(5,0)
    }
  }

  "A miss frame -5 " should {
    "return a Frame(0,5) " in {
      Frame.build("-5") shouldBe Frame(0,5)
    }
  }

  "A miss frame -- " should {
    "return a Frame(0,0) " in {
      Frame.build("--") shouldBe Frame(0,0)
    }
  }





}
