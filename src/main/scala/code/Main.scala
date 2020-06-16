package code

sealed trait Frame {
  def value: Seq[Int]
}
case class Strike() extends Frame {
  override def value = Array(10)
}
case class Other(r1: Int, r2: Int = 0) extends Frame {
  override def value = Array(r1,r2)
}


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
      str.toArray match {
        case Array(n) => Other(charToDigit(n))
        case Array(n, '/') => Other(charToDigit(n), 10 - charToDigit(n))
        case Array(n, m) => Other(charToDigit(n), charToDigit(m))
      }
    }
  }
}

object Main extends App {

  def calculateScore(game:String):Int = {

    val values: Array[Int] = (game.split(" ") map {x => Frame.build(x).value}).flatten

    def getValues(n: Int): Int = values(n) + values(n+1) + values(n+2)

    (game.split(" ") map {Frame.build}).slice(0,10).zipWithIndex.map {
      case (o, n) if o.value.sum == 10 =>
        getValues(n)
      case (o, _) =>
        o.value.sum
      case _ =>
        0
    }.sum

  }

}

