object Series {
  def slices(length: Int, string: String): List[List[Int]] =
    string.sliding(length).map(item => item.toList.map(_.toString.toInt)).toList
}
