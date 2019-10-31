object SpaceAge {
  def onEarth(age: Double) = age / 31557600
  def onMercury(age: Double) = onEarth(age) / 0.2408467
  def onVenus(age: Double) = onEarth(age) / 0.61519726
  def onMars(age: Double) = onEarth(age) / 1.8808158
  def onJupiter(age: Double) = onEarth(age) / 11.862615
  def onSaturn(age: Double) = onEarth(age) / 29.447498
  def onUranus(age: Double) = onEarth(age) / 84.016846
  def onNeptune(age: Double) = onEarth(age) / 164.79132
}
