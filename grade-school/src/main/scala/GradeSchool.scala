import scala.collection.SortedMap

class GradeSchool {
  type DB = SortedMap[Int, Seq[String]]
  var DataBase: SortedMap[Int, Seq[String]] = SortedMap[Int, Seq[String]]()

  def add(name: String, g: Int): DB = {
    DataBase += (g -> (DataBase.getOrElse(g, Seq()) ++ Seq(name)))
    DataBase
  }

  def db: DB = DataBase

  def grade(g: Int): Seq[String] = DataBase.getOrElse(g, Seq())

  def sorted: DB = DataBase.mapValues(_.sorted)
}
