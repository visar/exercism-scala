import scala.annotation.tailrec

object MatchingBrackets {
  def isPaired(brackets: String): Boolean =
    isPaired(brackets.toList, List[Char]())

  @tailrec
  def isPaired(brackets: List[Char], stack: List[Char]): Boolean =
    brackets match {
      case head :: tail =>
        if (openingBrackets.contains(head))
          isPaired(tail, head :: stack)
        else if (closingBrackets.contains(head))
          stack.headOption.contains(inverse(head)) && isPaired(tail, stack.tail)
        else
          isPaired(tail, stack)
      case Nil =>
        stack.isEmpty
    }

  private val openingBrackets: Seq[Char] = Seq('[', '(', '{')
  private val closingBrackets: Seq[Char] = Seq(']', ')', '}')

  private val inverse: Char => Char = {
    case ']' => '['
    case ')' => '('
    case '}' => '{'
    case x   => x
  }
}
