class GradeSchool {
  type DB = Map[Int, Seq[String]]
  var DataBase: DB = Map[Int, Seq[String]]()

  def add(name: String, g: Int): DB = {
    DataBase += (g -> (DataBase.getOrElse(g, Seq()) ++ Seq(name)))
    DataBase
  }

  def db: DB = DataBase

  def grade(g: Int): Seq[String] = DataBase.getOrElse(g, Seq())

  def sorted: DB = Map(DataBase.toSeq.sortBy(_._1): _*).mapValues(_.sorted)
}
