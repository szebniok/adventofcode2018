import scala.io.Source
import scala.annotation.tailrec

object day12 extends App {
  case class Pot(id: Long, containsPlant: Boolean)
  type Pattern = List[Boolean]

  def convertToken(token: Char): Boolean = token match {
    case '#' => true
    case '.' => false
  }

  def convertPots(pots: List[Pot]): String =
    pots
      .map {
        case Pot(_, false) => '.'
        case Pot(_, true)  => '#'
      }
      .mkString("")

  def parseInput(input: List[String]): (List[Pot], Map[Pattern, Boolean]) = {
    val stateLine :: _ :: patternLines = input

    val state =
      stateLine
        .split(" ")
        .drop(2)
        .apply(0)
        .zipWithIndex
        .map {
          case (token, index) =>
            Pot(index, convertToken(token))
        }
        .toList

    val patterns =
      patternLines
        .map(_.split(" => ").toList)
        .map {
          case p :: r :: Nil =>
            (p.map(convertToken(_)).toList, convertToken(r(0)))
        }
        .toMap

    (state, patterns)
  }

  def newGeneration(state: List[Pot],
                    patterns: Map[Pattern, Boolean]): List[Pot] = {
    val minIndex = state(0).id
    val maxIndex = state.last.id

    val prefix = List(Pot(minIndex - 3, false),
                      Pot(minIndex - 2, false),
                      Pot(minIndex - 1, false))
    val suffix = List(Pot(maxIndex + 1, false),
                      Pot(maxIndex + 2, false),
                      Pot(maxIndex + 3, false))
    val newState = prefix ++ state ++ suffix

    newState
      .sliding(5)
      .map { l =>
        val pot = patterns.getOrElse(l.map(_.containsPlant), false)
        val index = l(2).id
        Pot(index, pot)
      }
      .toList
      .dropWhile(!_.containsPlant)
      .reverse
      .dropWhile(!_.containsPlant)
      .reverse

  }

  @tailrec
  def goToGeneration(state: List[Pot],
                     patterns: Map[Pattern, Boolean],
                     generation: Long,
                     i: Long = 0,
                     prev: Option[List[Pot]] = None): List[Pot] =
    if (i == generation) state
    else
      prev match {
        case Some(prevState)
            if convertPots(prevState) == convertPots(state) => {
          val offset = state(0).id - prevState(0).id
          state
            .map { case Pot(id, s) => Pot(id + offset * (generation - i), s) }
        }
        case _ =>
          goToGeneration(newGeneration(state, patterns),
                         patterns,
                         generation,
                         i + 1,
                         Some(state))

      }

  val (initialState, patterns) = parseInput(
    Source.fromFile("day12input.txt").getLines.toList)
  println(
    goToGeneration(initialState, patterns, 20)
      .filter(_.containsPlant)
      .map(_.id)
      .sum)

  // 2 star
  val fiftyBillion = 50000000000L
  println(
    goToGeneration(initialState, patterns, fiftyBillion)
      .filter(_.containsPlant)
      .map(_.id)
      .sum)

}
