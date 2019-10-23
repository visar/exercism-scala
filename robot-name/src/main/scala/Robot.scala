import scala.util.Random

case class Robot() {
  var name: String = generate()

  def reset(): Unit = {
    this.name = generate()
  }

  def generate(): String = (Random.shuffle(('A' to 'Z').toList.take(2)) ++ Seq.fill(3)(Random.nextInt(10))).mkString("")
}
