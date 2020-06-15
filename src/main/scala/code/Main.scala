package code


case class Frame(roll1:Int, roll2:Int)
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
      Frame(10, 0)
    else {
      (str(0), str(1)) match {
        case (n, '/') => Frame(charToDigit(n), 10 - charToDigit(n))
        case (n, m) => Frame(charToDigit(n), charToDigit(m))
      }
    }
  }
}

object Main extends App {

  def calculateScore(game:String):Int = {

    game.split(" ") map Frame.build
    
    300
  }


  val gameString = "X X X X X X X X X X X X"



}

