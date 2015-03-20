package otg.mechsoul.graphc

import scala.collection.mutable.Queue
import scala.io.Source

import otg.mechsoul.graphc.cp.ConstraintEngine
import otg.mechsoul.graphc.cp.Result
import otg.mechsoul.graphc.graph.Edge
import otg.mechsoul.graphc.graph.Graph

class Solver(val graph: Graph) {
  val cp: ConstraintEngine = new ConstraintEngine(graph)
  val queue = new Queue[Result]

  def solveIt(): Unit = {
    var x = 0
    while (x < 0) {
      x = x + 1

      val choices = cp.giveNextChoices()
      val goodChoice = choices.toStream.map(ch => cp.applyChoice(ch)).find(_.success)

      if (goodChoice.isDefined) {
        queue += goodChoice.get
      } else {
        queue.dequeue.rollback
      }
    }

    val t = Node(Choice(1, 0),
      List(
        Node(Choice(0, 0), Nil),
        Node(Choice(0, 1),
          List(
            Node(Choice(2, 0), Nil),
            Node(Choice(2, 1), Nil))),
        Node(Choice(0, 2), Nil)))

    Tree.fold[Int](t, 0, (i: Int, ch: Choice) => {
      println(ch)
      0
    })
  }

}

case class Choice(val vertex: Int, val color: Int) {
  override def toString() = s"($vertex with $color)"
}

sealed abstract class Tree
case class Node(choice: Choice, children: List[Node]) extends Tree
case object TNil extends Tree

object Tree {
  def fold[B](t: Tree, z: B, f: (B, Choice) => B): B = t match {
    case Node(ch, children) => {
      f(children.foldLeft(z)((b: B, child: Node) => fold(child, b, f)), ch)
    }
    case _ => z
  }
  
}

object Solver {
  def main(args: Array[String]) {
    val graph = parseInput(readInputFile(args))
    println(graph)
    val solver = new Solver(graph)
    solver.solveIt()
  }

  private def readInputFile(args: Array[String]): Iterator[String] = {
    val filename = args.headOption.getOrElse("data/gc_4_1")
    Source.fromFile(filename).getLines
  }

  private def parseInput(lines: Iterator[String]): Graph = {
    val s = lines.next.split(' ');
    val nodeCount: Int = s(0).toInt;
    val edgeCount: Int = s(1).toInt;

    val edges = lines.map(line => {
      val l = line.split(' ')
      Edge(l(0).toInt, l(1).toInt)
    }).toArray

    return new Graph(nodeCount, edges);
  }
}