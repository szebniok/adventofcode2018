import scala.io.Source

object day3 extends App {
  case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int)
  def readClaim(line: String) = {
    val claim = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r
    line match {
      case claim(id, x, y, w, h) =>
        Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
    }
  }
  val claims: List[Claim] =
    Source.fromFile("day3input.txt").getLines().map(readClaim(_)).toList
  val occurences =
    claims
      .foldLeft(Map[(Int, Int), Int]()) {
        case (occ, claim) =>
          val fields: Seq[(Int, Int)] =
            for {
              xs <- claim.x until claim.x + claim.w
              ys <- claim.y until claim.y + claim.h
            } yield (xs, ys)
          fields.foldLeft(occ) {
            case (occ, f) =>
              val n: Int = occ.getOrElse(f, 0) + 1
              occ + (f -> n)
          }
      }
  println(occurences.filter { case (_, num) => num > 1 }.size)

  // 2 star
  claims
    .filter { claim =>
      val fields: Seq[(Int, Int)] =
        for {
          xs <- claim.x until claim.x + claim.w
          ys <- claim.y until claim.y + claim.h
        } yield (xs, ys)
      fields.forall(occurences.getOrElse(_, 0) == 1)
    }
    .foreach(println)
}
