sealed trait Planet {
  val orbitalPeriod: Double
}

object Earth extends Planet {
  val SecondsInYear = 31557600
  val orbitalPeriod: Double = 1.0

  def secondsToYears(age: Double): Double = age / Earth.SecondsInYear
}

object Mercury extends Planet {
  val orbitalPeriod: Double = 0.2408467
}

object Venus extends Planet {
  val orbitalPeriod: Double = 0.61519726
}

object Mars extends Planet {
  val orbitalPeriod: Double = 1.8808158
}

object Jupiter extends Planet {
  val orbitalPeriod: Double = 11.862615
}

object Saturn extends Planet {
  val orbitalPeriod: Double = 29.447498
}

object Uranus extends Planet {
  val orbitalPeriod: Double = 84.016846
}

object Neptune extends Planet {
  val orbitalPeriod: Double = 164.79132
}

object SpaceAge {
  def apply(planet: Planet, age: Double): Double = Earth.secondsToYears(age) / planet.orbitalPeriod

  def onEarth(age: Double): Double = SpaceAge(Earth, age)
  def onMercury(age: Double): Double = SpaceAge(Mercury, age)
  def onVenus(age: Double): Double = SpaceAge(Venus, age)
  def onMars(age: Double): Double = SpaceAge(Mars, age)
  def onJupiter(age: Double): Double = SpaceAge(Jupiter, age)
  def onSaturn(age: Double): Double = SpaceAge(Saturn, age)
  def onUranus(age: Double): Double = SpaceAge(Uranus, age)
  def onNeptune(age: Double): Double = SpaceAge(Neptune, age)
}
