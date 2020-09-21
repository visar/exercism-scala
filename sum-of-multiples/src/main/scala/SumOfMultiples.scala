object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int =
    factors.flatMap { factor =>
      Stream.from(1).takeWhile(_ * factor < limit).map(_ * factor)
    }.sum
}
