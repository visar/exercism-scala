object Darts {
  def score(x: Double, y: Double): Int = {
    val distance = math.sqrt(x * x + y * y)

    if (distance == 0) 10
    else if (distance <= 5) 5
    else if (distance <= 10) 1
    else 0
  }
}
