object RnaTranscription {
  val DnaToRna = Map(
    'G' -> 'C',
    'C' -> 'G',
    'T' -> 'A',
    'A' -> 'U'
  )

  def toRna(strand: String): Option[String] =
    Option(strand.map(DnaToRna(_)))
}
