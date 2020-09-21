import scala.annotation.tailrec

object BeerSong {
  def recite(bottles: Int, verses: Int): String = {
    @tailrec
    def recite(bottles: Int, verses: Int, current: Int, poem: String): String = {
      if (current == verses)
        poem
      else
        recite(
          bottles - 1,
          verses,
          current + 1,
          poem ++ "\n" * (if (current == 0 || current == verses) 0 else 1) ++ verse(bottles)
        )
    }

    recite(bottles, verses, current = 0, poem = "")
  }

  def verse(bottles: Int): String = bottles match {
    case 0 =>
      s"No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
    case 1 =>
      s"1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
    case 2 =>
      s"2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"
    case _ =>
      s"$bottles bottles of beer on the wall, $bottles bottles of beer.\nTake one down and pass it around, ${bottles - 1} bottles of beer on the wall.\n"
  }
}
