object DifferenceOfSquares {

  def sumOfSquares(n: Int): Int = (1 to n).foldLeft(0)((acc, n) => acc + n * n)

  def squareOfSum(n: Int): Int = {
    val s = (1 to n).sum
    s * s
  }

  def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)
}
