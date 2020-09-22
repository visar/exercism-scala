import scala.annotation.tailrec

object AllYourBase {
  def rebase(from: Int, number: List[Int], to: Int): Option[List[Int]] = {
    @tailrec
    def fromDec(number: Int, base: Int, acc: List[Int]): List[Int] = {
      if (number == 0) acc
      else fromDec(number / base, base, number % base :: acc)
    }

    if (from <= 1 || to <= 1 || number.exists(_ < 0) || number.exists(_ >= from)) None
    else {
      number.foldLeft(0) {
        case (acc, n) => from * acc + n
      }
    } match {
      case 0 => Some(List(0))
      case n => Some(fromDec(n, to, List()))
    }
  }
}
