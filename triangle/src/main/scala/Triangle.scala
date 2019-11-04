case class Triangle(a: Double, b: Double, c: Double) {
  def isLegal: Boolean =
    if (a == 0 || b == 0 || c == 0) false
    else if (a + b <= c) false
    else if (a + c <= b) false
    else if (c + b <= a) false
    else true

  def equilateral: Boolean =
    isLegal && (a == b) && (b == c)

  def isosceles: Boolean =
    isLegal && (a == b || b == c || a == c)

  def scalene: Boolean =
    isLegal && !isosceles
}
