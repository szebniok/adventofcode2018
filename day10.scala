import scala.io.Source
import scala.annotation.tailrec

object day10 extends App {
  case class Point(x: Int, y: Int, xV: Int, yV: Int)

  def parsePoint(input: String): Point = {
    val numbers =
      input
        .replace("position=<", "")
        .replace("> velocity=<", ",")
        .replace(">", "")
        .split(",")
        .map(_.trim.toInt)
    Point(numbers(0), numbers(1), numbers(2), numbers(3))
  }

  def noSingleDisjointPoint(points: List[Point]): Boolean = {
    val taken =
      points
        .map(p => (p.x, p.y))
        .toSet

    points
      .map(
        p =>
          taken.contains(p.x, p.y - 1) ||
            taken.contains(p.x + 1, p.y - 1) ||
            taken.contains(p.x + 1, p.y) ||
            taken.contains(p.x + 1, p.y + 1) ||
            taken.contains(p.x, p.y + 1) ||
            taken.contains(p.x - 1, p.y + 1) ||
            taken.contains(p.x - 1, p.y) ||
            taken.contains(p.x - 1, p.y - 1))
      .forall(identity)
  }

  @tailrec
  def arrange(points: List[Point], time: Int = 0): (List[Point], Int) =
    if (noSingleDisjointPoint(points)) (points, time)
    else {
      arrange(points
                .map(p => Point(p.x + p.xV, p.y + p.yV, p.xV, p.yV)),
              time + 1)

    }

  def printPoints(points: List[Point]): Unit = {
    val minX = points.map(_.x).min
    val minY = points.map(_.y).min
    val maxX = points.map(_.x).max
    val maxY = points.map(_.y).max

    val coords = for {
      y <- minY to maxY
      x <- minX to maxX
    } yield (x, y)

    coords
      .foreach {
        case (x, y) =>
          val point = points.find(p => p.x == x && p.y == y)
          point match {
            case None    => print(".")
            case Some(_) => print("#")
          }
          if (x == maxX) println()
      }
  }

  val input =
    Source
      .fromFile("day10input.txt")
      .getLines
      .map(parsePoint)
      .toList

  printPoints(arrange(input)._1)

  // 2 star
  println(arrange(input)._2)
}
