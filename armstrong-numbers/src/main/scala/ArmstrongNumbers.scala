import scala.annotation.tailrec

object ArmstrongNumbers {
  def isArmstrongNumber(input: Int): Boolean = {
    val digs = digits(input)
    val numberOfDigits = digs.length
    val checkSum = digs.fold(0)(_ + Math.pow(_, numberOfDigits).toInt)

    checkSum == input
  }

  def digits(input: Int): List[Int] = {
    @tailrec
    def digits(input: Int, digs: List[Int]): List[Int] = {
      if (input == 0) digs
      else digits(input / 10, (input % 10) :: digs)
    }

    digits(input, List())
  }
}
