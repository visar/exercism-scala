import scala.collection.mutable
import scala.util.Random

case class Robot() {
  var name: String = generate()

  def reset(): Unit = {
    this.name = generate()
  }

  def generate(): String = {
    Robot.cache.generate()
  }
}

object Robot {
  val cache: SimpleSetCache = SimpleSetCache(mutable.HashSet.empty[String])
}

sealed trait Cache[A] {
  def seen(input: A): Boolean

  def generate(): A
}

case class SimpleSetCache(cache: mutable.HashSet[String]) extends Cache[String] {
  def seen(input: String): Boolean = cache contains input

  def raw(): String = (Random.shuffle(('A' to 'Z').toList.take(2)) ++ Seq.fill(3)(Random.nextInt(10))).mkString

  def generate(): String = {
    var candidate: String = raw()
    while (seen(candidate))
      candidate = raw()

    cache += candidate

    candidate
  }
}

