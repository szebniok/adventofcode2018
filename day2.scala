import scala.io.Source

object day2 extends App {
  val idOccurences: List[Map[Char, Int]] =
    Source.fromFile("day2input.txt")
          .getLines()
          .toList
          .map(_.groupBy(identity).mapValues(_.length))

  val twoTimes = idOccurences.map(_.count { case (char, num) => num == 2 }).count(_ != 0)
  val threeTimes = idOccurences.map(_.count { case (char, num) => num == 3 }).count(_ != 0)
  println(twoTimes * threeTimes)

  // 2 star
  val lines: List[String] = Source.fromFile("day2input.txt").getLines().toList
  val lineLength: Int = lines(0).length
  val result: String = (0 until lineLength)
    .flatMap(index => lines.map(line => line.substring(0, index) + '0' + line.substring(index + 1)))
    .groupBy(identity).mapValues(_.length)
    .filter{ case(char, num) => num > 1 }
    .keys.toArray.apply(0)
  println(result.filterNot(_ == '0'))
}
