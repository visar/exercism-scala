case class WordCount(sentence: String) {
  def countWords(): Map[String, Int] =
    sentence.toLowerCase
      .filterNot("!&@$%^:.".contains(_))
      .split(Array(' ', ',', '\n'))
      .filterNot(_ == "")
      .map { x =>
        if (x.head == '\'' && x.last == '\'')
          x.slice(1, x.length - 1)
        else x
      }
      .groupBy(identity)
      .mapValues(_.length)
}
