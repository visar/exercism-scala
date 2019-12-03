import scala.collection.SortedMap

class GradeSchool {
  type DB = Map[Int, Seq[String]]
  var DataBase: SortedMap[Int, Seq[String]] = SortedMap[Int, Seq[String]]()

  def add(name: String, g: Int): Unit = {
    DataBase += (g -> (DataBase.getOrElse(g, Seq()) ++ Seq(name)))
  }

  def db: DB = DataBase.toMap

  def grade(g: Int): Seq[String] = DataBase.getOrElse(g, Seq())

  def sorted: DB = DataBase.mapValues(_.sorted).toMap
}
