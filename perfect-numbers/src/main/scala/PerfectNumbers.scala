import scala.annotation.tailrec

sealed trait NumberType

case object Perfect extends NumberType
case object Abundant extends NumberType
case object Deficient extends NumberType

object PerfectNumbers {
  def classify(num: Int): Either[String, NumberType] = {
    if (num <= 0)
      Left("Classification is only possible for natural numbers.")
    else {
      val divs = divisors(num)
      if (divs.sum == num)
        Right(Perfect)
      else if (divs.sum > num)
        Right(Abundant)
      else Right(Deficient)
    }
  }

  def divisors(num: Int): List[Int] = {
    @tailrec
    def divisors(num: Int, current: Int, divs: List[Int]): List[Int] = {
      if (current == num) {
        divs
      } else if (num % current == 0) {
        divisors(num, current + 1, current :: divs)
      }
      else divisors(num, current + 1, divs)
    }

    divisors(num, 1, List[Int]())
  }
}
