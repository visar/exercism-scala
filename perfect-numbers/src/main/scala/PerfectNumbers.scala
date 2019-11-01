import scala.annotation.tailrec

object NumberType extends Enumeration {
  val Deficient = Value(-1)
  val Perfect = Value(0)
  val Abundant = Value(1)
}

object PerfectNumbers {
  def classify(num: Int): Either[String, NumberType.Value] = {
    if (num <= 0)
      Left("Classification is only possible for natural numbers.")
    else {
      Right(NumberType(divisors(num).sum compare num))
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
