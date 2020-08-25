import scala.annotation.tailrec

object NumberType extends Enumeration {
  val Deficient = Value(-1)
  val Perfect = Value(0)
  val Abundant = Value(1)
}

object PerfectNumbers {
  def classify(num: Int): Either[String, NumberType.Value] =
    if (num <= 0)
      Left("Classification is only possible for natural numbers.")
    else
      Right(NumberType(divisors(num).sum compare num))

  def divisors(num: Int): List[Int] =
    (1 to (num / 2)).filter(num % _ == 0).toList
}
