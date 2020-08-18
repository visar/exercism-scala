object FlattenArray {
  def flatten(list: List[Any]): List[Int] = {
    def flatten(list: List[Any], acc: List[Int]): List[Int] = list match {
      case (head: Int) :: tail => flatten(tail, acc :+ head)
      case (head: List[Any]) :: tail => flatten(tail, acc ++ flatten(head, List.empty[Int]))
      case _ :: tail => flatten(tail, acc)
      case Nil => acc
    }

    flatten(list, List.empty[Int])
  }
}