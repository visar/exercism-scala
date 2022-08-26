sealed trait SimpleLinkedList[+T] {
  def isEmpty: Boolean
  def value: T
  def add[T1 >: T](item: T1): SimpleLinkedList[T1]
  def next: SimpleLinkedList[T]
  def reverse: SimpleLinkedList[T]
  def toSeq: Seq[T]
}

object Empty extends SimpleLinkedList[Nothing] {
  override def isEmpty: Boolean                                = true
  override def value                                           = throw new NoSuchElementException
  override def add[T](item: T): SimpleLinkedList[T] = NonEmpty(item, Empty)
  override def next: SimpleLinkedList[Nothing]                 = throw new NoSuchElementException
  override def reverse: SimpleLinkedList[Nothing]              = this
  override def toSeq: Seq[Nothing]                             = Nil
}

case class NonEmpty[T](value: T, next: SimpleLinkedList[T]) extends SimpleLinkedList[T] {
  override def isEmpty: Boolean                             = false
  override def add[T1 >: T](item: T1): SimpleLinkedList[T1] = NonEmpty(value, next.add(item))
  override def reverse: SimpleLinkedList[T]                 = next.reverse.add(value)
  override def toSeq: Seq[T]                                = value +: next.toSeq
}

object SimpleLinkedList {
  def apply[T](): SimpleLinkedList[T]              = Empty
  def apply[T](elements: T*): SimpleLinkedList[T]  = fromSeq(elements)
  def fromSeq[T](seq: Seq[T]): SimpleLinkedList[T] = seq.foldLeft(SimpleLinkedList[T]())((list, el) => list.add(el))
}
