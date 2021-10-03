import scala.annotation.tailrec

object ArmstrongNumbers {
  def isArmstrongNumber(input: Int): Boolean = {
    val digs           = input.toString
    val numberOfDigits = digs.length
    val checkSum       = digs.foldLeft(0)((acc, ch) => acc + Math.pow(ch.asDigit, numberOfDigits).toInt)

    checkSum == input
  }
}
