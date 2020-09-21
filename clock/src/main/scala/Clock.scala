import Math.{floorDiv, floorMod}

class Clock(var hours: Int, var minutes: Int) {
  def +(that: Clock): Clock = Clock(hours + that.hours, minutes + that.minutes)

  def -(that: Clock): Clock = Clock(hours - that.hours, minutes - that.minutes)

  override def equals(that: Any): Boolean =
    that match {
      case c: Clock => this.hours == c.hours && this.minutes == c.minutes
      case _        => false
    }

  override def toString: String = s"Clock($hours, $minutes)"
}

object Clock {
  def apply(hours: Int, minutes: Int): Clock =
    new Clock(floorMod(hours + floorDiv(minutes, 60), 24), floorMod(minutes, 60))

  def apply(minutes: Int): Clock = Clock(0, minutes)
}
