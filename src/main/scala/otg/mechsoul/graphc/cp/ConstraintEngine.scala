package otg.mechsoul.graphc.cp

import otg.mechsoul.graphc.graph.Graph

class ConstraintEngine(val graph: Graph) {

  private val NO_COLOR = -1

  val colors: Array[Int] = Array.fill(graph.nodeCount)(NO_COLOR)
  val vertexDomains: Array[List[Int]] = (0 until graph.nodeCount).map(vertex => List(0, 1, 2, 3)).toArray

  def checkEdges(): Boolean = {
    graph.edges.forall(edge => {
      val fromColor = color(edge.from)
      val toColor = color(edge.to)
      fromColor == NO_COLOR || toColor == NO_COLOR || fromColor != toColor
    })
  }

  def checkDomains(): Boolean = {
    (0 until graph.nodeCount).forall(vertex => !vertexDomains(vertex).isEmpty)
  }

  def color(vertex: Int): Int = colors(vertex)
  def setColor(vertex: Int, color: Int) = colors(vertex) = color

}

trait Instruction {
  def commit(cp: ConstraintEngine)
  def rollback(cp: ConstraintEngine)
}

case class SetVertexColor(vertex: Int, color: Int, prevColor: Int) extends Instruction {
  override def commit(cp: ConstraintEngine): Unit = {
    cp.setColor(vertex, color);
  }

  override def rollback(cp: ConstraintEngine): Unit = {
    cp.setColor(vertex, prevColor);
  }
}