import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: (A) => B, list: List[A]): List[B] = {
    @tailrec
    def accumulate[A, B](f: (A) => B, list: List[A], acc: List[B]): List[B] =
      list match {
        case head :: tail => accumulate(f, tail, f(head) :: acc)
        case Nil => acc.reverse
      }

    accumulate(f, list, List.empty[B])
  }
}
