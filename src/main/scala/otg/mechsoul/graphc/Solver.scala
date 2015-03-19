package otg.mechsoul.graphc

import otg.mechsoul.graphc.graph.Graph
import otg.mechsoul.graphc.graph.Edge

object Solver {
  def main(args: Array[String]) {

    val graph = new Graph(2, Array(Edge(0, 1)))
    println(graph)
    
  }
}