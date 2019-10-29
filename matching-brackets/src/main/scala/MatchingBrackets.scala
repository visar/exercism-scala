import scala.annotation.tailrec

object MatchingBrackets {
  def isPaired(brackets: String): Boolean = isPaired(brackets.toList, List[Char]())

  @tailrec
  def isPaired(brackets: List[Char], stack: List[Char]): Boolean = brackets match {
    case head :: tail =>
      if (Seq('[', '(', '{').contains(head))
        isPaired(tail, head :: stack)
      else if (Seq(']', ')', '}').contains(head))
        stack.nonEmpty && stack.head == inverse(head) && isPaired(tail, stack.tail)
      else
        isPaired(tail, stack)
    case Nil =>
      stack.isEmpty
  }

  private def inverse(bracket: Char): Char = bracket match {
    case ']' => '['
    case ')' => '('
    case '}' => '{'
    case x => x
  }
}
