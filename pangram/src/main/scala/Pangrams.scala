object Pangrams {
  def isPangram(input: String): Boolean =
    ('a' to 'z').intersect(input.map(_.toLower)) == ('a' to 'z')
}
