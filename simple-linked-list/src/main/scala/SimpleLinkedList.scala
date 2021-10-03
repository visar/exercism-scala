sealed trait SimpleLinkedList[T] {
  def isEmpty: Boolean
  def value: T
  def add(item: T): SimpleLinkedList[T]
  def next: SimpleLinkedList[T]
  def reverse: SimpleLinkedList[T]
  def toSeq: Seq[T]
}

case class Empty[T]() extends SimpleLinkedList[T] {
  override def isEmpty: Boolean                  = true
  override def value: T                          = throw new NoSuchElementException
  override def add(item: T): SimpleLinkedList[T] = NonEmpty(item, Empty())
  override def next: SimpleLinkedList[T]         = throw new IllegalStateException
  override def reverse: SimpleLinkedList[T]      = this
  override def toSeq: Seq[T]                     = Nil
}

case class NonEmpty[T](value: T, next: SimpleLinkedList[T]) extends SimpleLinkedList[T] {
  override def isEmpty: Boolean                  = false
  override def add(item: T): SimpleLinkedList[T] = NonEmpty(value, next.add(item))
  override def reverse: SimpleLinkedList[T]      = next.reverse.add(value)
  override def toSeq: Seq[T]                     = value +: next.toSeq
}

object SimpleLinkedList {
  def apply[T](): SimpleLinkedList[T]              = Empty[T]()
  def apply[T](elements: T*): SimpleLinkedList[T]  = fromSeq(elements)
  def fromSeq[T](seq: Seq[T]): SimpleLinkedList[T] = seq.foldLeft(SimpleLinkedList[T]())((list, el) => list.add(el))
}
