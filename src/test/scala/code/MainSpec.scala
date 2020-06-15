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
      Frame.build("X") shouldBe Strike()
    }
  }

  "A spare frame 5/ " should {
    "return a Frame(5,5) " in {
      Frame.build("5/") shouldBe Spare(5)
    }
  }

  "A miss frame 5- " should {
    "return a Frame(5,0) " in {
      Frame.build("5-") shouldBe OpenFrame(5,0)
    }
  }

  "A miss frame -5 " should {
    "return a Frame(0,5) " in {
      Frame.build("-5") shouldBe OpenFrame(0,5)
    }
  }

  "A miss frame -- " should {
    "return a Frame(0,0) " in {
      Frame.build("--") shouldBe OpenFrame(0,0)
    }
  }

  "scoreFrame(frames)" should {
    "return score of first frame if no strike or spare" in {
     Main.scoreFrame(List(OpenFrame(4, 3), OpenFrame(4, 3), OpenFrame(4, 3))) shouldBe 7
    }
  }

  "scoreFrame(frames) two" should {
    "return score of first frame if no strike or spare n" in {
     Main.scoreFrame(List(Spare(4), OpenFrame(4, 3), OpenFrame(4, 3))) shouldBe 14
    }
  }

  "scoreFrame(frames) two" should {
    "return score of first frame if no strike or spare m" in {
     Main.scoreFrame(List(Spare(4), OpenFrame(4, 3))) shouldBe 14
    }
  }



}
