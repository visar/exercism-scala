object Allergies {
  import Allergen._

  def allergicTo(allergen: Allergen, score: Int): Boolean = list(score).contains(allergen)

  def list(score: Int): List[Allergen] =
    score.toBinaryString.reverse
      .zip(
        List(
          Eggs,
          Peanuts,
          Shellfish,
          Strawberries,
          Tomatoes,
          Chocolate,
          Pollen,
          Cats
        )
      )
      .reverse
      .foldLeft(List.empty[Allergen]) {
        case (list: List[Allergen], (c: Char, allergen: Allergen)) =>
          if (c == '1') allergen :: list
          else list
      }
}

sealed trait Allergen {
  def score: Int
}

object Allergen {
  abstract class AbstractAllergen(val score: Int) extends Allergen
  final case object Eggs                          extends AbstractAllergen(1)
  final case object Peanuts                       extends AbstractAllergen(2)
  final case object Shellfish                     extends AbstractAllergen(4)
  final case object Strawberries                  extends AbstractAllergen(8)
  final case object Tomatoes                      extends AbstractAllergen(16)
  final case object Chocolate                     extends AbstractAllergen(32)
  final case object Pollen                        extends AbstractAllergen(64)
  final case object Cats                          extends AbstractAllergen(128)

  implicit val orderingAllergen: Ordering[Allergen] = Ordering.by(_.score)
}
