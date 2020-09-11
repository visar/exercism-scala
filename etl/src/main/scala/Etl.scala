object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] =
    m.keys.flatMap {
      score: Int => {
        m.get(score).map {
          values: Seq[String] =>
            values.map {
              value: String =>
                value.toLowerCase -> score
            }
        }
      }.get
    }.toMap
}