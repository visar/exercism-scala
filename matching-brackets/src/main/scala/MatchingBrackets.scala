import scala.annotation.tailrec

object MatchingBrackets {
  def isPaired(brackets: String): Boolean = isPaired(brackets.toList, List[Char]())

  @tailrec
  def isPaired(brackets: List[Char], stack: List[Char]): Boolean = brackets match {
    case '[' :: tail => isPaired(tail, '[' :: stack)
    case ']' :: tail => stack.nonEmpty && stack.head == '[' && isPaired(tail, stack.tail)
    case '(' :: tail => isPaired(tail, '(' :: stack)
    case ')' :: tail => stack.nonEmpty && stack.head == '(' && isPaired(tail, stack.tail)
    case '{' :: tail => isPaired(tail, '{' :: stack)
    case '}' :: tail => stack.nonEmpty && stack.head == '{' && isPaired(tail, stack.tail)
    case _ :: tail => isPaired(tail, stack)
    case Nil => stack.isEmpty
  }
}
