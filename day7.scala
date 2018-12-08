import scala.io.Source
import scala.annotation.tailrec

object day7 extends App {
  @tailrec
  def getOrder(steps: Map[Char, List[Char]],
               queue: List[Char],
               order: List[Char] = List()): String =
    queue match {
      case Nil => order.reverse.mkString("")
      case step :: tail => {
        val nextSteps: List[Char] = steps.getOrElse(step, List())
        val newSteps = steps - step

        val possibleMoves =
          nextSteps
            .filter { s =>
              newSteps.toList
                .forall(!_._2.contains(s))
            }

        val newQueue = (tail ++ possibleMoves).sorted

        getOrder(newSteps, newQueue, step :: order)
      }
    }

  val steps: Map[Char, List[Char]] =
    Source
      .fromFile("day7input.txt")
      .getLines()
      .map { line =>
        val l = line.filter(_.isUpper).drop(1)
        (l(0), l(1))
      }
      .toList
      .groupBy(_._1)
      .mapValues(_.map(_._2))

  val possibleStartingMoves =
    steps.toList
      .map(_._1)
      .toSet
      .filter { s =>
        steps.toList
          .forall(!_._2.contains(s))
      }
      .toList
      .sorted

  println(getOrder(steps, possibleStartingMoves))

  // 2 star
  def stepTime(step: Char) = step - 'A' + 61

  case class Worker(task: Option[Char], timeRemaining: Int)

  def assemblyTime(steps: Map[Char, List[Char]],
                   queue: List[Char],
                   idleWorkers: List[Worker],
                   busyWorkers: List[Worker],
                   time: Int): Int = {

    if (busyWorkers.isEmpty && queue.isEmpty) time
    else {
      val advancedWorkers =
        busyWorkers
          .map {
            case Worker(task, t) => Worker(task, t - 1)
            case w               => w
          }

      val (finishedWorkers, stillWorking) =
        advancedWorkers
          .partition {
            case Worker(t, 0) => true
            case _            => false
          }

      val finishedTasks: List[Char] = finishedWorkers.map(_.task.get)
      val newSteps = steps.filterKeys(!finishedTasks.contains(_))

      val newPossibleTasks =
        finishedTasks
          .flatMap(steps.getOrElse(_, List()))
          .filter { task =>
            newSteps.toList
              .forall(!_._2.contains(task))
          }

      val newQueue = (queue ++ newPossibleTasks).sorted
      val newWorkers =
        (idleWorkers ++ finishedWorkers)
          .zip(newQueue)
          .map { case (_, task) => Worker(Some(task), stepTime(task)) }

      val newIdleWorkers =
        (idleWorkers ++ finishedWorkers).drop(newWorkers.length)
      assemblyTime(newSteps,
                   newQueue.drop(newWorkers.length),
                   newIdleWorkers,
                   stillWorking ++ newWorkers,
                   time + 1)
    }
  }

  println(
    assemblyTime(steps,
                 possibleStartingMoves,
                 List.fill(5)(Worker(None, 0)),
                 List(),
                 -1))
}
