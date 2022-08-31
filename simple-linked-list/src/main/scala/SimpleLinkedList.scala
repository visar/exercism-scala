sealed trait SimpleLinkedList[+T] {
  def isEmpty: Boolean
  def value: T
  def add[T1 >: T](item: T1): SimpleLinkedList[T1]
  def next: SimpleLinkedList[T]
  def reverse: SimpleLinkedList[T]
  def toSeq: Seq[T]
}

object Empty extends SimpleLinkedList[Nothing] {
  override def isEmpty: Boolean                     = true
  override def value                                = throw new NoSuchElementException
  override def add[T](item: T): SimpleLinkedList[T] = SimpleLinkedList(item)
  override def next: SimpleLinkedList[Nothing]      = throw new NoSuchElementException
  override def reverse: SimpleLinkedList[Nothing]   = this
  override def toSeq: Seq[Nothing]                  = Seq.empty
}

case class Cons[T](head: T, tail: SimpleLinkedList[T]) extends SimpleLinkedList[T] {
  override val value: T                                     = head
  override val next: SimpleLinkedList[T]                    = tail
  override def isEmpty: Boolean                             = false
  override def add[T1 >: T](item: T1): SimpleLinkedList[T1] = Cons(head, tail.add(item))
  override def reverse: SimpleLinkedList[T]                 = tail.reverse.add(head)
  override def toSeq: Seq[T]                                = head +: tail.toSeq
}

object SimpleLinkedList {
  def apply[T](): SimpleLinkedList[T]              = Empty
  def apply[T](elements: T*): SimpleLinkedList[T]  = fromSeq(elements)
  def fromSeq[T](seq: Seq[T]): SimpleLinkedList[T] = seq.foldRight(SimpleLinkedList[T]())(Cons(_, _))
}
