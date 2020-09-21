object Hamming {
  def distance(first: String, second: String): Option[Int] =
    if (first.length != second.length)
      None
    else
      Some(first.zip(second).count {
        case (x, y) => x != y
      })
}
