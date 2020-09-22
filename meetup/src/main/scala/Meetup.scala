import java.time.{DayOfWeek, LocalDate, YearMonth}

import Schedule.Schedule

case class Meetup(month: Int, year: Int) {
  def day(dayOfWeek: Int, schedule: Schedule): LocalDate = schedule match {
    case Schedule.Teenth =>
      days(start = 13, end = 19, dayOfWeek = dayOfWeek).head
    case day @ (Schedule.First | Schedule.Second | Schedule.Third | Schedule.Fourth) =>
      days(start = 7 * (day.id - 1) + 1, end = 7 * day.id, dayOfWeek = dayOfWeek).head
    case Schedule.Last =>
      days(start = 22, end = YearMonth.of(year, month).lengthOfMonth(), dayOfWeek = dayOfWeek).last
  }

  def days(start: Int, end: Int, dayOfWeek: Int): Seq[LocalDate] =
    (start to end)
      .map(day => LocalDate.of(year, month, day))
      .filter(_.getDayOfWeek.getValue == dayOfWeek)
      .slice(0, 2)
}

object Schedule extends Enumeration {
  type Schedule = Value
  val Teenth, First, Second, Third, Fourth, Last = Value
}

object Meetup {
  val Mon: Int = DayOfWeek.MONDAY.getValue
  val Tue: Int = DayOfWeek.TUESDAY.getValue
  val Wed: Int = DayOfWeek.WEDNESDAY.getValue
  val Thu: Int = DayOfWeek.THURSDAY.getValue
  val Fri: Int = DayOfWeek.FRIDAY.getValue
  val Sat: Int = DayOfWeek.SATURDAY.getValue
  val Sun: Int = DayOfWeek.SUNDAY.getValue
}
