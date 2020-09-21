import scala.annotation.tailrec

object PrimeFactors {
  def factors(input: Long): List[Long] = {
    @tailrec
    def factors(queue: List[Long], list: List[Long]): List[Long] = {
      if (queue.isEmpty)
        list.reverse
      else if (isPrime(queue.head)) {
        factors(queue.tail, queue.head :: list)
      } else {
        val pair: List[Long] = (2 to Math.sqrt(queue.head).toInt)
          .filter(queue.head % _ == 0)
          .take(1)
          .flatMap(x => List(x, queue.head / x))
          .toList
        factors(queue.tail ++ pair, list)
      }
    }

    input match {
      case 1 => List()
      case _ => factors(List(input), List())
    }
  }

  def isPrime(input: Long): Boolean = {
    (2 to Math.sqrt(input).toInt).forall(input % _ != 0)
  }
}
