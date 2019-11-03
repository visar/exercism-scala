import scala.annotation.tailrec

object CollatzConjecture {
  def steps(input: Int): Option[Int] = {
    @tailrec
    def steps(input: Int, count: Int): Option[Int] = {
      if (input <= 0) None
      else if (input == 1) Some(count)
      else if (input % 2 == 0) steps(input / 2, count + 1)
      else steps(3 * input + 1, count + 1)
    }

    steps(input, 0)
  }
}
