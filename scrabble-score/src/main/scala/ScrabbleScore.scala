object ScrabbleScore {
  def score(input: String): Int = {
    val scoringMap: Map[Char, Int] = Map(
      Set('A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T') -> 1,
      Set('D', 'G')                                         -> 2,
      Set('B', 'C', 'M', 'P')                               -> 3,
      Set('F', 'H', 'V', 'W', 'Y')                          -> 4,
      Set('K')                                              -> 5,
      Set('J', 'X')                                         -> 8,
      Set('Q', 'Z')                                         -> 10
    ).flatMap { case (s, i) => s.map(e => e -> i) }

    input.foldLeft(0)((acc, char) => acc + scoringMap(char.toUpper))
  }
}
