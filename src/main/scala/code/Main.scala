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
    if (str == "X") {
      Strike()
    } else if(str.length==1) {
      OpenFrame(charToDigit(str(0)), 0)
    }
    else {
      (str(0), str(1)) match {
        case (n, '/') => Spare(charToDigit(n))
        case (n, m) => OpenFrame(charToDigit(n), charToDigit(m))
      }
    }
  }
}

object Main extends App {


  def scoreFrame(frame1:Frame, frame2:Option[Frame], frame3:Option[Frame]):Int = {
    (frame1, frame2, frame3) match {
      case (OpenFrame(n, m), _, _) =>  n + m
      case (Strike(), Some(Strike()), Some(f3)) => {
        f3 match {
          case Strike() => 30
          case Spare(n) => 20 + n
          case OpenFrame(n, _) => 20 + n
        }
      }
      case (Strike(), Some(f2), _) => {
        f2 match {
          case Spare(_) => 20
          case OpenFrame(n, m) => 10 + n + m
        }
      }
      case (Spare(_), Some(f2), _) => {
        f2 match {
          case Strike() => 20
          case Spare(n) => 10 + n
          case OpenFrame(n, _) => 10 + n
        }
      }
    }
  }

  def calculateScore(game:String):Int = {

    val frames = game.split(" ") map Frame.build
    val blanks = List.fill(12-frames.size)(OpenFrame(0,0))
    val framesList = frames.toList ++ blanks

    framesList.sliding(3, 1).map { window =>
      scoreFrame(window.head, window.tail.headOption, window.tail.tail.headOption)
    }.toList.sum
  }


}

