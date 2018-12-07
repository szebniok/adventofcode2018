import scala.io.Source

object day6 extends App {
  sealed trait FieldType
  case object Indeterminate extends FieldType
  case class Claimed(id: Int) extends FieldType

  def manhattanDistance(a: (Int, Int), b: (Int, Int)): Int =
    math.abs(a._1 - b._1) + math.abs(a._2 - b._2)

  val locations: List[((Int, Int), Int)] =
    Source
      .fromFile("day6input.txt")
      .getLines()
      .map(_.split(", ").map(_.toInt))
      .map { case Array(x, y) => (x, y) }
      .toList
      .zipWithIndex

  val width: Int = locations.map(_._1._1).max
  val height: Int = locations.map(_._1._2).max

  val borders: IndexedSeq[(Int, Int)] =
    (0 to width).map((_, 0)) ++
      (0 to width).map((_, height)) ++
      (0 to height).map((0, _)) ++
      (0 to height).map((width, _))

  val points: IndexedSeq[(Int, Int)] =
    for {
      y <- 0 to height
      x <- 0 to width
    } yield (x, y)

  val computedPoints = points
    .map { point =>
      val distances =
        locations
          .map {
            case (location, index) =>
              (manhattanDistance(point, location), index)
          }
          .sortBy { case (distance, _) => distance }

      if (distances(0)._1 == distances(1)._1) (point, Indeterminate)
      else (point, Claimed(distances(0)._2))
    }

  val plane: Seq[IndexedSeq[((Int, Int), FieldType)]] =
    computedPoints.grouped(width + 1).toSeq

  val remove = borders
    .map {
      case (x, y) => plane(y)(x)
    }
    .map {
      case (_, Claimed(id)) => id
      case _                => -1
    }
    .toSet

  println(
    computedPoints
      .filter {
        case (_, Claimed(id)) => !remove.contains(id)
        case _                => false
      }
      .groupBy(_._2)
      .mapValues(_.length)
      .maxBy(_._2)
      ._2)

  // 2 star
  println(
    points
      .map(point =>
        locations.map(location => manhattanDistance(location._1, point)).sum)
      .filter(_ < 10000)
      .length)

}
