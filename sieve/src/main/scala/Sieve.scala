object Sieve {
  def primes(n: Int): List[Int] = {
    val factors: Seq[Int] = (2 to math.sqrt(n).toInt).flatMap { v =>
      Stream.from(2).takeWhile(_ * v <= n).map(_ * v)
    }

    (2 to n).toList.filterNot(factors.contains(_))
  }
}
