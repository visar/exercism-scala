case class DNA(strand: String) {
  def nucleotideCounts: Either[String, Map[Char, Int]] = {
    if (strand.filter("ACGT".contains(_)) == strand)
      Right(
        strand.toList
          .groupBy(identity)
          .mapValues(_.length)
          .foldLeft(Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0)) {
            case (m, v) => m.updated(v._1, v._2)
          }
      )
    else Left("Invalid nucleotides")
  }
}
