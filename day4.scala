import scala.io.Source

object day4 extends App {
  case class Shift(id: Int, sleepStart: Int, sleepEnd: Int)

  def logToShift(log: List[String]): List[Shift] =
    log match {
      case logId :: tail => {
        val guardId: Int =
          logId.split(" ").filter(_.startsWith("#")).apply(0).substring(1).toInt
        val shifts: List[Shift] =
          tail
            .takeWhile(!_.contains("Guard"))
            .grouped(2)
            .map {
              case logStart :: logEnd :: Nil =>
                val sleepStart = logStart.substring(15, 17).toInt
                val sleepEnd = logEnd.substring(15, 17).toInt
                Shift(guardId, sleepStart, sleepEnd)
            }
            .toList
        shifts ++ logToShift(tail.dropWhile(!_.contains("Guard")))
      }
      case Nil => List()
    }

  val sortedInput: List[String] = Source
    .fromFile("day4input.txt")
    .getLines()
    .toList
    .sorted

  val shifts: List[Shift] = logToShift(sortedInput)

  val (guardId, _) =
    shifts
      .groupBy(_.id)
      .mapValues(_.map(shift => shift.sleepEnd - shift.sleepStart))
      .mapValues(_.sum)
      .maxBy(_._2)

  val (maxMinute, _) =
    shifts
      .filter(_.id == guardId)
      .flatMap(shift => shift.sleepStart until shift.sleepEnd)
      .groupBy(identity)
      .mapValues(_.length)
      .maxBy(_._2)

  println(guardId * maxMinute)

  // 2 star
  val (mostCommonId, (mostCommonMinute, frequency)) =
    shifts
      .groupBy(_.id)
      .mapValues { shifts =>
        shifts
          .flatMap(shift => shift.sleepStart until shift.sleepEnd)
          .groupBy(identity)
          .mapValues(_.length)
          .maxBy(_._2)
      }
      .maxBy { case (id, (minute, frequency)) => frequency }

  println(mostCommonId * mostCommonMinute)
}
