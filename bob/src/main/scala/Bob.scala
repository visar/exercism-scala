object Bob {
  def response(statement: String): String = {
    if (statement.trim == "")
      "Fine. Be that way!"
    else if (statement.toUpperCase == statement &&
      statement.trim.endsWith("?") &&
      statement.intersect('A' to 'Z').nonEmpty)
      "Calm down, I know what I'm doing!"
    else if (statement.toUpperCase == statement
      && statement.intersect('A' to 'Z').nonEmpty)
      "Whoa, chill out!"
    else if (statement.trim.endsWith("?"))
      "Sure."
    else
      "Whatever."
  }
}
