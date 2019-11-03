import scala.annotation.tailrec

object House {
  def recite(startVerse: Int, endVerse: Int): String = {
    @tailrec
    def recite(startVerse: Int, endVerse: Int, acc: String): String = {
      if (startVerse > endVerse)
        acc ++ "\n"
      else {
        val verse =
          startVerse match {
            case 1 => "This is the house that Jack built.\n"
            case 2 => "This is the malt that lay in the house that Jack built.\n"
            case 3 => "This is the rat that ate the malt that lay in the house that Jack built.\n"
            case 4 => "This is the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 5 => "This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 6 => "This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 7 => "This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 8 => "This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 9 => "This is the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 10 => "This is the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 11 => "This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
            case 12 => "This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.\n"
          }
        recite(startVerse + 1, endVerse, acc ++ verse)
      }
    }

    recite(startVerse, endVerse, "")
  }
}
