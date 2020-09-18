object HighScores {
  def latest(scores: List[Int]): Int = scores.last

  def personalBest(scores: List[Int]): Int = scores.max

  def personalTop(scores: List[Int]): List[Int] = scores.sorted(Ordering[Int].reverse).slice(0, 3)

  def report(scores: List[Int]): String = {
    val bestScore = personalBest(scores)
    val latestScore = latest(scores)
    if (bestScore == latestScore)
      s"Your latest score was $latestScore. That's your personal best!"
    else
      s"Your latest score was $latestScore. That's ${bestScore - latestScore} short of your personal best!"
  }
}