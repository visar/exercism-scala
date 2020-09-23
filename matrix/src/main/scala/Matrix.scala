trait MatrixTrait {
  def rows: Array[Array[Int]]
  def columns: Array[Array[Int]]
}

case class Matrix(string: String) extends MatrixTrait {
  val rows: Array[Array[Int]]        = string.split("\n").map(_.split(" ").map(_.toInt))
  val columns: Array[Array[Int]]     = rows.transpose
  def row(index: Int): Array[Int]    = rows(index)
  def column(index: Int): Array[Int] = columns(index)
}
