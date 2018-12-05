import scala.io.Source
import scala.annotation.tailrec

object day5 extends App {
  val polymer: String =
    Source.fromFile("day5input.txt").getLines().toList.apply(0)

  @tailrec
  def react(polymer: String): String = {
    val newPolymer =
      polymer.toList
        .foldLeft("") { (result, ch) =>
          if (result.length > 0) {
            val lastChar = result.last
            if (lastChar.toLower == ch.toLower && lastChar != ch)
              result.init
            else
              result + ch
          } else ch.toString
        }
    if (newPolymer.length < polymer.length) react(newPolymer)
    else newPolymer
  }
  println(react(polymer).length)

  // 2 star
  println(
    ('a' to 'z')
      .map(unit => polymer.filter(_.toLower != unit))
      .map(react)
      .map(_.length)
      .min)
}
