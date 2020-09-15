object Raindrops {

  implicit class Factor(number: Int) {
    def isFactorOf(factor: Int): Boolean = number % factor == 0
  }

  def convert(implicit n: Int): String = {
    var str: String = ""
    n isFactorOf 3
    if (n isFactorOf 3) str += "Pling"
    if (n isFactorOf 5) str += "Plang"
    if (n isFactorOf 7) str += "Plong"
    if (str != "") str else n.toString
  }
}

