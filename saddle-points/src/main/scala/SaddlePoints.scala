case class Matrix(repr: List[List[Int]]) {
  def saddlePoints: Set[(Int, Int)] =
    if (repr.head.isEmpty) Set()
    else {
      val u = for {
        r <- repr
      } yield r.max

      val v = for {
        r <- repr.transpose
      } yield r.min

      (for {
        (x, i) <- u.zipWithIndex
        (y, j) <- v.zipWithIndex
        if x == y
      } yield (i, j)).toSet
    }
}
