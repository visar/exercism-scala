object PascalsTriangle {
  def rows(row_number: Int): List[List[Int]] = {
    if (row_number <= 0)
      List()
    else
      row_number match {
        case 1 => List(List(1))
        case n =>
          val previous = rows(n - 1)
          val lastRow  = previous.last

          previous :+
            (0 :: lastRow, lastRow :+ 0).zipped.map { (a, b) =>
              a + b
            }
      }
  }
}
