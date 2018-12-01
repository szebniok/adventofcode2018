import scala.io.Source
import scala.annotation.tailrec

object day1 extends App {
  println(Source.fromFile("day1input.txt").getLines().map(_.toInt).foldLeft(0)(_ + _))

  // 2 star
  @tailrec
  def findDuplicate[A](stream: Stream[A], seen: Set[A] = Set[A]()): A = {
    stream match {
      case head #:: tail => 
        if (seen contains head) head
        else findDuplicate(tail, seen + head) 
    }
  }

  val values: Stream[String] = Source.fromFile("day1input.txt").getLines().toStream #::: values
  var seen: Set[Int] = Set()
  println(findDuplicate(values.map(_.toInt).scanLeft(0)(_ + _)))
}
