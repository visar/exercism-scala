import scala.annotation.tailrec

object FoodChain {
  def recite(startVerse: Int, endVerse: Int): String = {
    @tailrec
    def recite(startVerse: Int, endVerse: Int, acc: String): String = {
      if (startVerse > endVerse) {
        acc
      } else {
        val verse =
          startVerse match {
            case 1 => """I know an old lady who swallowed a fly.
                        |I don't know why she swallowed the fly. Perhaps she'll die.
                        |
                        |""".stripMargin

            case 2 => """I know an old lady who swallowed a spider.
                        |It wriggled and jiggled and tickled inside her.
                        |She swallowed the spider to catch the fly.
                        |I don't know why she swallowed the fly. Perhaps she'll die.
                        |
                        |""".stripMargin

            case 3 => """I know an old lady who swallowed a bird.
                        |How absurd to swallow a bird!
                        |She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
                        |She swallowed the spider to catch the fly.
                        |I don't know why she swallowed the fly. Perhaps she'll die.
                        |
                        |""".stripMargin

            case 4 => """I know an old lady who swallowed a cat.
                        |Imagine that, to swallow a cat!
                        |She swallowed the cat to catch the bird.
                        |She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
                        |She swallowed the spider to catch the fly.
                        |I don't know why she swallowed the fly. Perhaps she'll die.
                        |
                        |""".stripMargin

            case 5 => """I know an old lady who swallowed a dog.
                        |What a hog, to swallow a dog!
                        |She swallowed the dog to catch the cat.
                        |She swallowed the cat to catch the bird.
                        |She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
                        |She swallowed the spider to catch the fly.
                        |I don't know why she swallowed the fly. Perhaps she'll die.
                        |
                        |""".stripMargin

            case 6 => """I know an old lady who swallowed a goat.
                        |Just opened her throat and swallowed a goat!
                        |She swallowed the goat to catch the dog.
                        |She swallowed the dog to catch the cat.
                        |She swallowed the cat to catch the bird.
                        |She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
                        |She swallowed the spider to catch the fly.
                        |I don't know why she swallowed the fly. Perhaps she'll die.
                        |
                        |""".stripMargin

            case 7 => """I know an old lady who swallowed a cow.
                        |I don't know how she swallowed a cow!
                        |She swallowed the cow to catch the goat.
                        |She swallowed the goat to catch the dog.
                        |She swallowed the dog to catch the cat.
                        |She swallowed the cat to catch the bird.
                        |She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.
                        |She swallowed the spider to catch the fly.
                        |I don't know why she swallowed the fly. Perhaps she'll die.
                        |
                        |""".stripMargin

            case 8 => """I know an old lady who swallowed a horse.
                        |She's dead, of course!
                        |
                        |""".stripMargin
          }
        recite(startVerse + 1, endVerse, acc ++ verse)
      }
    }

    recite(startVerse, endVerse, "")
  }
}
