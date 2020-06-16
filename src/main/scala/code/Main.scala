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

  def charToInt(in: Char): Int =
    in.toString match {
      case "-" => 0
      case x => x.toInt
    }

  def build(str: String): Frame = {
    if (str == "X")
      Strike()
    else {
      str.toArray match {
        case Array(n) => Other(charToInt(n))
        case Array(n, '/') => Other(charToInt(n), 10 - charToInt(n))
        case Array(n, m) => Other(charToInt(n), charToInt(m))
      }
    }
  }
}

object Main extends App {

  def calculateScore(in:String):Int = {

    // clean up the spacing of Spare
    val game = in.replaceAll("(/){1}([0-9])$", "/ $2")

    // build array of values
    val values: Array[Int] = (game.split(" ") map {x => Frame.build(x).value}).flatten

    // build indexed list of Frame(s), calculate value of each, and sum
    (game.split(" ") map {Frame.build}).slice(0,10).zipWithIndex.map {
      case (frame, i) if frame.value.sum == 10 =>
        values(i) + values(i+1) + values(i+2)
      case (frame, _) =>
        frame.value.sum
      case _ =>
        0
    }.sum

  }

}

