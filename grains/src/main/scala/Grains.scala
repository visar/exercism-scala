object Grains {
  val total: BigInt = 2 * square(64).get - 1

  def square(n: Int): Option[BigInt] =
    if (n <= 0 || n > 64)
      None
    else if (n == 1)
      Some(1)
    else
      square(n - 1).map(_ * 2)
}