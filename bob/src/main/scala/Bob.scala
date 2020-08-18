sealed trait Statement {
  def response(): String
}

object Statement {

  case class YellingQuestion(statement: String) extends Statement {
    def response(): String = "Calm down, I know what I'm doing!"
  }

  case class Yelling(statement: String) extends Statement {
    def response(): String = "Whoa, chill out!"
  }

  case class Question(statement: String) extends Statement {
    def response(): String = "Sure."
  }

  case class Address(statement: String) extends Statement {
    def response(): String = "Fine. Be that way!"
  }

  case class Anything(statement: String) extends Statement {
    def response(): String = "Whatever."
  }

  def create(statement: String): Statement = YellingQuestion.create(statement)

  object YellingQuestion {
    def create(statement: String): Statement =
      if (statement.toUpperCase == statement && statement.trim.endsWith("?") && statement.intersect('A' to 'Z').nonEmpty)
        YellingQuestion(statement)
      else
        Yelling.create(statement)
  }

  object Yelling {
    def create(statement: String): Statement =
      if (statement.toUpperCase == statement && statement.intersect('A' to 'Z').nonEmpty)
        Yelling(statement)
      else
        Question.create(statement)
  }

  object Question {
    def create(statement: String): Statement =
      if (statement.trim.endsWith("?"))
        Question(statement)
      else
        Address.create(statement)
  }

  object Address {
    def create(statement: String): Statement =
      if (statement.trim == "")
        Address(statement)
      else
        Anything.create(statement)
  }

  object Anything {
    def create(statement: String): Anything = Anything(statement)
  }

}

object Bob {
  def response(str: String): String = Statement.create(str).response()
}