sealed trait Plant
object Plant {
  final case object Radishes extends Plant
  final case object Clover   extends Plant
  final case object Grass    extends Plant
  final case object Violets  extends Plant
}

trait GardenTrait {
  def columns: Seq[Seq[Char]]
}

case class Garden private (garden: String) extends GardenTrait {
  import Garden._

  def columns: List[List[Char]] = garden.split("\n").map(s => s.toList).toList

  def plants(name: String): List[Plant] = {
    val student = Student.withName(name)
    columns.flatMap(_.slice(student.from, student.to)).map(mapping)
  }
}

sealed trait Student {
  def from: Int
  def to: Int
}

object Student {
  abstract class AbstractStudent(val from: Int, val to: Int) extends Student
  case object Alice                                          extends AbstractStudent(0, 2)
  case object Bob                                            extends AbstractStudent(2, 4)
  case object Charlie                                        extends AbstractStudent(4, 6)
  case object David                                          extends AbstractStudent(6, 8)
  case object Eve                                            extends AbstractStudent(8, 10)
  case object Fred                                           extends AbstractStudent(10, 12)
  case object Ginny                                          extends AbstractStudent(12, 14)
  case object Harriet                                        extends AbstractStudent(14, 16)
  case object Ileana                                         extends AbstractStudent(16, 18)
  case object Joseph                                         extends AbstractStudent(18, 20)
  case object Kincaid                                        extends AbstractStudent(20, 22)
  case object Larry                                          extends AbstractStudent(22, 24)

  def withName(name: String): Student = name match {
    case "Alice"   => Alice
    case "Bob"     => Bob
    case "Charlie" => Charlie
    case "David"   => David
    case "Eve"     => Eve
    case "Fred"    => Fred
    case "Ginny"   => Ginny
    case "Harriet" => Harriet
    case "Ileana"  => Ileana
    case "Joseph"  => Joseph
    case "Kincaid" => Kincaid
    case "Larry"   => Larry
  }
}

object Garden {
  import Plant._

  def defaultGarden(garden: String): Garden = Garden(garden)

  def mapping: Map[Char, Plant] = Map(
    'R' -> Radishes,
    'C' -> Clover,
    'G' -> Grass,
    'V' -> Violets
  )
}
