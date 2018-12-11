import scala.io.Source

object day11 extends App {
  def hundredsDigit(n: Int): Int = (n / 100) % 10

  def powerLevel(gridSerialNumber: Int, x: Int, y: Int): Int =
    hundredsDigit((((x + 10) * y) + gridSerialNumber) * (x + 10)) - 5

  def maxSubarray(array: IndexedSeq[Int], size: Int): (Int, Int) =
    array
      .sliding(size)
      .zipWithIndex
      .map { case (numbers, index) => (numbers.sum, index + 1) }
      .maxBy(_._1)

  def maxPowerLevel(grid: Array[Array[Int]], size: Int): ((Int, Int), Int) = {
    grid
      .sliding(size)
      .zipWithIndex
      .map {
        case (rows, y) =>
          val colsSum =
            (0 until grid.length)
              .map(index => rows.map(_(index)).sum)

          val (maxSum, x) = maxSubarray(colsSum, size)
          ((x, y + 1), maxSum)
      }
      .maxBy(_._2)
  }

  val gridSerialNumber =
    Source.fromFile("day11input.txt").getLines.mkString.toInt

  val gridWidth = 300
  val gridHeight = 300

  val grid: Array[Array[Int]] = (for {
    y <- 1 to gridWidth
    x <- 1 to gridHeight
  } yield (x, y))
    .map { case (x, y) => powerLevel(gridSerialNumber, x, y) }
    .grouped(gridWidth)
    .map(_.toArray)
    .toArray

  println(maxPowerLevel(grid, 3))

  // 2 star
  println(
    (1 to gridWidth).par
      .map(size => (maxPowerLevel(grid, size), size))
      .maxBy(_._1._2))
}
