object Leap {
  def leapYear(year: Int): Boolean = {
    if (year % 400 == 0) true
    else if (year % 100  == 0) false
    else if (year % 4 == 0) true
    else false
  }
}
