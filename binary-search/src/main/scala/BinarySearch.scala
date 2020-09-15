import scala.annotation.tailrec

object BinarySearch {
  def find(list: List[Int], key: Int): Option[Int] = {
    @tailrec
    def find(start: Int, end: Int): Option[Int] =
      if (!list.contains(key))
        None
      else if (key == list((start + end) / 2))
        Some((start + end) / 2)
      else if (key < list((start + end) / 2))
        find(start, (start + end) / 2)
      else
        find((start + end) / 2 + 1, end)

    find(start = 0, end = list.length - 1)
  }
}