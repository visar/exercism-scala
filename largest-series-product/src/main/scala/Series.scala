object Series {
  def largestProduct(length: Int, digits: String): Option[Int] =
    if (length > digits.length) None
    else if (length < 0) None
    else if (!digits.forall(Character.isDigit)) None
    else if (length == 0) Some(1)
    else
      Some((for {
        d <- digits.sliding(length)
      } yield d.toList.foldLeft(1)((acc, c) => acc * (c.toInt - '0'.toInt))).max)
}
