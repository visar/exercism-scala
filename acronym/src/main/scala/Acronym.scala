object Acronym {
  def abbreviate(phrase: String): String =
    phrase
      .replace("-", " ")
      .split(" +")
      .map(_.head.toUpper)
      .mkString
}
