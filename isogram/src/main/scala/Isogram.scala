object Isogram {
  val lowercase: Seq[Char] = 'a' to 'z'

  def isIsogram(word: String): Boolean =
    word.toLowerCase
      .filter(lowercase.contains(_))
      .groupBy(identity)
      .mapValues(_.length)
      .values
      .count(_ > 1) == 0
}
