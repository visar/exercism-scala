object Anagram {
  def count(word: String): Map[Char, Int] =
    word.toLowerCase.toList.groupBy(identity).mapValues(_.length)

  def findAnagrams(word: String, listOfWords: List[String]): List[String] =
    for {
      w <- listOfWords
      v = count(word)
      if w.toLowerCase != word.toLowerCase && count(w) == v
    } yield w
}
