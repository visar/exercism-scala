import java.time.LocalDate
import java.time.LocalDateTime

object Gigasecond {
  val gigaseconds = 1000000000

  def add(startDate: LocalDate): LocalDateTime =
    add(startDate.atStartOfDay())

  def add(startDateTime: LocalDateTime): LocalDateTime =
    startDateTime.plusSeconds(gigaseconds)
}
