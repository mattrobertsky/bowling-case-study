package code

sealed trait Frame
case class OpenFrame(roll1:Int, roll2:Int) extends Frame
case class Strike() extends Frame
case class Spare(roll1:Int) extends Frame

object Frame {

  def charToDigit(ch: Char): Int =
    ch match {
      case '-' => 0
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
    }

  def build(str: String): Frame = {
    if (str == "X")
      Strike()
    else {
      (str(0), str(1)) match {
        case (n, '/') => Spare(charToDigit(n))
        case (n, m) => OpenFrame(charToDigit(n), charToDigit(m))
      }
    }
  }
}

object Main extends App {


  def scoreFrame(threeFrames: List[Frame]):Int = {
    threeFrames match {
      case Strike() :: Strike() :: Strike() :: Nil => 30
      case Strike() :: Strike() :: Spare(n) :: Nil => 20 + n
      case Strike() :: Strike() :: OpenFrame(n, _) :: Nil => 20 + n
      case Strike() :: Spare(_)  :: _ => 20
      case Strike() :: OpenFrame(n, m) :: Nil => 10 + n + m
      case Spare(_) :: Strike() :: _ => 20
      case Spare(_) :: Spare(n) :: _ => 10 + n
      case Spare(_) :: OpenFrame(n, _) :: _ => 10 + n
      case OpenFrame(n, m) :: _  => n + m
    }
  }

  def calculateScore(game:String):Int = {

    val frames = game.split(" ") map Frame.build
    val framesList = frames.toList
    framesList.sliding(3, 1).map(scoreFrame).toList.sum
  }


}

