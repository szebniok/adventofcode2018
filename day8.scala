import scala.io.Source
import scala.annotation.tailrec

object day8 extends App {
  case class Node(numNodes: Int,
                  numMetadata: Int,
                  childs: List[Node],
                  entries: List[Int])
  val input = Source.fromFile("day8input.txt").getLines().toList.mkString(" ")

  def parseTree(input: String): (Node, String) = {
    val numbers = input.split(" ").map(_.toInt)
    val numNodes = numbers(0)
    val numMetadata = numbers(1)

    if (numNodes == 0)
      (
        Node(numNodes,
             numMetadata,
             List(),
             numbers.drop(2).take(numMetadata).toList),
        numbers.drop(2 + numMetadata).mkString(" ")
      )
    else {
      var remainingInput = numbers.drop(2).mkString(" ")
      var nodes: List[Node] = List()
      while (remainingInput
               .split(" ")
               .length > numMetadata && nodes.length < numNodes) {
        val (newNode, rest) = parseTree(remainingInput)
        nodes ++= List(newNode)
        remainingInput = rest
      }
      val entries =
        remainingInput.split(" ").take(numMetadata).map(_.toInt).toList
      remainingInput = remainingInput.split(" ").drop(numMetadata).mkString(" ")
      (Node(numNodes, numMetadata, nodes, entries), remainingInput)
    }
  }

  def sumMetadata(tree: Node): Int =
    tree.entries.sum + tree.childs.map(sumMetadata(_)).sum

  val (tree, _) = parseTree(input)

  println(sumMetadata(tree))

  // 2 star
  def value(tree: Node): Int =
    if (tree.numNodes == 0) tree.entries.sum
    else
      tree.entries
        .filter(_ <= tree.numNodes)
        .map(index => tree.childs(index - 1))
        .map(value(_))
        .sum

  println(value(tree))
}
