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
    var best = Int.MaxValue
    var bestColoring: Array[Int] = new Array[Int](graph.nodeCount)
    var b = true

    while (b) {
      if (choicer.resultsSize > 0) {
        if (ch.color < graph.nodeCount) {
          ch = choicer.failure(ch)
        } else {
          val r = choicer.popLastResult
          println(r)
          r.rollback()
          ch = choicer.failure(r.choice)
        }

        if (ch.color > graph.nodeCount) {
          b = false
        }
      }

      println(choicer.resultsSize() + " " + cp.colorsCount + " " + graph.nodeCount + " ,best=" + best)
      while (choicer.resultsSize() < graph.nodeCount && cp.colorsCount < best) {
        ch = choicer.next(ch)
      }

      if (choicer.resultsSize() == graph.nodeCount && cp.checkEdges()) {
        val cc = cp.colorsCount
        // println(s"found coloring is $cc")
        if (best > cc) {
          best = cp.colorsCount
          Array.copy(cp.colors, 0, bestColoring, 0, graph.nodeCount)
          // println(s"minimal cosloring is: $cc")
        }
      }
    }

    val bestSolution = Solution(Int.MaxValue, Array(), Nil)
    val firstChoice = new Choice(choicer.firstVertex, 0)

    // println(s"colors: " + cp.colors.distinct.length)
    // println(cp.checkEdges())

    println(s"$best 0")
    println(bestColoring.mkString(" "))
  }

  private def findSolution(from: Choice, best: Solution): Option[Solution] = {
    None
  }

  var traversed = 0
  def traverse(color: Int, vertexIdx: Int, best: Int): Int = {
    traversed = traversed + 1
    
    if(traversed % 250 == 0) {
     // println(traversed)
    }
    
    var bestSoFar = best
    if (vertexIdx < graph.nodeCount && cp.colorsCount < bestSoFar) {
      val ch = new Choice(cp.graph.deg(vertexIdx)._2, color)

      // println("-" * vertexIdx + " " + ch)

      val result = cp.applyChoice(ch)
      if (result.success) {
        for (i <- 0 until graph.nodeCount) {
          bestSoFar = traverse(i, vertexIdx + 1, bestSoFar)
        }
      }

      if (vertexIdx + 1 == graph.nodeCount && cp.checkFeasibility()) {
        if (cp.colorsCount < bestSoFar) {
          bestSoFar = cp.colorsCount
        }
        println(cp.colorsCount + " " + cp.colors.mkString(","))

      }

      result.rollback()

    }

    bestSoFar

  }

}

case class Solution(val colorCount: Int, val colors: Array[Int], results: List[Result])

case class Choice(val vertex: Int, val color: Int) {
  override def toString() = s"($vertex with $color)"
}

object Solver {
  
  def main(args: Array[String]) {
    val graph = parseInput(readInputFile(args))
    // println(graph)
    val solver = new Solver(graph)
    // solver.solveIt()
    solver.traverse(0, 0, Int.MaxValue)
  }

  private def readInputFile(args: Array[String]): Iterator[String] = {
    val filename = args.headOption.getOrElse("data/gc_70_1")
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