object Raindrops {
  def isFactorOf(number: Int, factor: Int): Boolean = number % factor == 0

  def convert(n: Int): String = {
    n match {
      case n if isFactorOf(n, 3) && isFactorOf(n, 5) && isFactorOf(n, 7) => "PlingPlangPlong"
      case n if isFactorOf(n, 3) && isFactorOf(n, 5) => "PlingPlang"
      case n if isFactorOf(n, 3) && isFactorOf(n, 7) => "PlingPlong"
      case n if isFactorOf(n, 5) && isFactorOf(n, 7) => "PlangPlong"
      case n if isFactorOf(n, 3) => "Pling"
      case n if isFactorOf(n, 5) => "Plang"
      case n if isFactorOf(n, 7) => "Plong"
      case n => n.toString
    }
  }
}

