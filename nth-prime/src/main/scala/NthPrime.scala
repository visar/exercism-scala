object NthPrime {
  def isPrime(n: Int): Boolean =
    !(2 to math.sqrt(n).toInt).exists(n % _ == 0)

  def prime(n: Int): Option[Int] = {
    if (n > 0) {
      Some(
        (for {
          v <- Stream.from(2)
          if isPrime(v)
        } yield v)
          .take(n)
          .last
      )
    } else None
  }
}
