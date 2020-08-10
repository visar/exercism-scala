sealed trait Statement {
  def response(): String =
    this match {
      case Statement.Question(_: String) => "Sure."
      case Statement.Yelling(_: String) => "Whoa, chill out!"
      case Statement.YellingQuestion(_: String) => "Calm down, I know what I'm doing!"
      case Statement.Address(_: String) => "Fine. Be that way!"
      case Statement.Anything(_: String) => "Whatever."
    }
}

object Statement {
  def create(str: String): Statement = {
    val yellingQuestion = YellingQuestion.create(str)
    yellingQuestion match {
      case Some(question) => question
      case None =>
        val yelling = Yelling.create(str)
        yelling match {
          case Some(yelling) => yelling
          case None =>
            val question = Question.create(str)
            question match {
              case Some(yellingQuestion) => yellingQuestion
              case None =>
                val address = Address.create(str)
                address match {
                  case Some(address) => address
                  case None => Anything(str)
                }
            }
        }
    }
  }

  case class Question private(statement: String) extends Statement
  object Question {
    def create(statement: String): Option[Question] =
      if (statement.trim.endsWith("?")) Some(Question(statement)) else None
  }

  case class Yelling private(statement: String) extends Statement
  object Yelling {
    def create(statement: String): Option[Yelling] =
      if (statement.toUpperCase == statement
        && statement.intersect('A' to 'Z').nonEmpty)
        Some(Yelling(statement)) else None
  }

  case class YellingQuestion private(statement: String) extends Statement
  object YellingQuestion {
    def create(statement: String): Option[YellingQuestion] =
      if (statement.toUpperCase == statement && statement.trim.endsWith("?") && statement.intersect('A' to 'Z').nonEmpty)
        Some(YellingQuestion(statement))
      else
        None
  }

  case class Address private(statement: String) extends Statement
  object Address {
    def create(statement: String): Option[Address] =
      if (statement.trim == "") Some(Address(statement)) else None
  }

  case class Anything(statement: String) extends Statement
}

object Bob {
  def response(str: String): String = Statement.create(str).response()
}
