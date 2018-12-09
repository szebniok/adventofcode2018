import scala.io.Source
import scala.annotation.tailrec

object day9 extends App {
  case class Marble(value: Long, var prev: Marble, var next: Marble)

  def nextPlayer(player: Int) = player % playersCount + 1

  @tailrec
  def generateScores(currentMarble: Marble,
                     nextMarbleNumber: Int,
                     player: Int,
                     playersCount: Int,
                     highestMarbleScore: Int,
                     playerScores: Map[Int, Long] = Map().withDefaultValue(0))
    : Map[Int, Long] =
    if (nextMarbleNumber % 23 == 0) {
      var marbleToBeDeleted = currentMarble
      var i = 0
      while (i < 7) {
        marbleToBeDeleted = marbleToBeDeleted.prev
        i += 1
      }

      marbleToBeDeleted.prev.next = marbleToBeDeleted.next
      marbleToBeDeleted.next.prev = marbleToBeDeleted.prev

      val marbleScore = nextMarbleNumber + marbleToBeDeleted.value

      val newPlayerScores =
        playerScores.updated(player, playerScores(player) + marbleScore)

      if (marbleScore == highestMarbleScore) {
        println("aha")
        newPlayerScores
      } else
        generateScores(marbleToBeDeleted.next,
                       nextMarbleNumber + 1,
                       nextPlayer(player),
                       playersCount,
                       highestMarbleScore,
                       newPlayerScores)
    } else if (nextMarbleNumber == highestMarbleScore) playerScores
    else {
      val clockwiseMarble = currentMarble.next
      val newMarble =
        Marble(nextMarbleNumber, clockwiseMarble, clockwiseMarble.next)
      clockwiseMarble.next = newMarble
      newMarble.next.prev = newMarble

      generateScores(newMarble,
                     nextMarbleNumber + 1,
                     nextPlayer(player),
                     playersCount,
                     highestMarbleScore,
                     playerScores)
    }

  val input = Source.fromFile("day9input.txt").getLines.mkString.split(" ")
  val playersCount = input(0).toInt
  val highestMarbleScore = input(6).toInt

  val initialMarble: Marble = Marble(0, null, null)
  initialMarble.prev = initialMarble
  initialMarble.next = initialMarble

  println(
    generateScores(initialMarble, 1, 0, playersCount, highestMarbleScore)
      .maxBy(_._2)
      ._2)

  // 2 star
  val initialMarblePart2: Marble = Marble(0, null, null)
  initialMarblePart2.prev = initialMarblePart2
  initialMarblePart2.next = initialMarblePart2
  println(
    generateScores(initialMarblePart2,
                   1,
                   0,
                   playersCount,
                   highestMarbleScore * 100).maxBy(_._2)._2)
}
