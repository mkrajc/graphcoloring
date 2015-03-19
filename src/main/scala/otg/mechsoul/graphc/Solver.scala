package otg.mechsoul.graphc

import otg.mechsoul.graphc.graph.Graph
import otg.mechsoul.graphc.graph.Edge
import scala.io.Source
import otg.mechsoul.graphc.cp.ConstraintEngine
import otg.mechsoul.graphc.cp.ConstraintEngine

class Solver(val graph: Graph) {
  val cp: ConstraintEngine = new ConstraintEngine(graph)

  def solveIt(): Unit = {
    val feasible = cp.checkEdges
    println(feasible)
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