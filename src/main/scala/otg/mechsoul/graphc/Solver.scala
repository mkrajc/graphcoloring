package otg.mechsoul.graphc

import scala.collection.mutable.Queue
import scala.io.Source
import otg.mechsoul.graphc.cp.ConstraintEngine
import otg.mechsoul.graphc.cp.Result
import otg.mechsoul.graphc.graph.Edge
import otg.mechsoul.graphc.graph.Graph
import otg.mechsoul.graphc.cp.ChoiceProvider
import otg.mechsoul.graphc.cp.GreedyChoicer
import otg.mechsoul.graphc.cp.GreedySmartChoicer

class Solver(val graph: Graph) {
  val cp: ConstraintEngine = new ConstraintEngine(graph)

  def solveIt(): Unit = {
    val choicer = new GreedySmartChoicer(cp)
    var ch = new Choice(choicer.firstVertex, 0)

    while (choicer.currentChoices.length < graph.nodeCount) {
      ch = choicer.next(ch)
    }
    
    // println(s"colors: " + cp.colors.distinct.length)
    // println(cp.checkEdges())
    
    println(s"${cp.colors.distinct.length} 0")
    println(choicer.cp.colors.mkString(" "))
  }

}

case class Choice(val vertex: Int, val color: Int) {
  override def toString() = s"($vertex with $color)"
}

object Solver {
  def main(args: Array[String]) {
    val graph = parseInput(readInputFile(args))
    // println(graph)
    val solver = new Solver(graph)
    solver.solveIt()
  }

  private def readInputFile(args: Array[String]): Iterator[String] = {
    val filename = args.headOption.getOrElse("data/gc_500_9")
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