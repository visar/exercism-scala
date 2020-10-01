object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] =
    m.flatMap {
      case (score, letters) => letters.map(_.toLowerCase -> score)
    }
}
