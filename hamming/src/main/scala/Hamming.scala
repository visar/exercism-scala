object Hamming {
  def distance(first: String, second: String): Option[Int] =
    if (first.length != second.length)
      None
    else
      Some(first.zip(second).foldLeft(0) {
        (acc: Int, pair: (Char, Char)) =>
          if (pair._1 != pair._2) acc + 1
          else acc
      })
}