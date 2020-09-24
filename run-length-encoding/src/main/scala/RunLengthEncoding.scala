object RunLengthEncoding {
  def encode(message: String): String = {
    message
      .foldLeft(List.empty[Map[Char, Int]]) {
        case (acc, char) =>
          if (acc.nonEmpty && acc.last.head._1 == char)
            acc.init :+ Map(char -> (acc.last(char) + 1))
          else
            acc :+ Map(char -> 1)
      }
  }.map { m =>
      val char: Char = m.head._1
      val count: Int = m.head._2
      if (count == 1)
        char.toString
      else
        count.toString ++ char.toString
    }
    .mkString("")

  def decode(message: String): String = {
    message
      .foldLeft((List.empty[Map[Char, Int]], "")) {
        case ((acc, count), char) =>
          char match {
            case ch if ch.isLetter || ch.isSpaceChar =>
              if (count != "")
                (acc :+ Map(ch -> count.toInt), "")
              else
                (acc :+ Map(ch -> 1), "")
            case n if n.isDigit =>
              (acc, count :+ n)
          }
      }
      ._1
      .map { m =>
        List.fill(m.head._2)(m.head._1).mkString
      }
      .mkString
  }
}
