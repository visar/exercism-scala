object Strain {
  def keep[A](input: Seq[A], p: => A => Boolean): Seq[A] = {
    var acc = Seq.empty[A]
    for (i <- input)
      if (p(i))
        acc = acc :+ i

    acc
  }

  def discard[A](input: Seq[A], p: => A => Boolean): Seq[A] =
    keep[A](input, !p(_))
}