package otg.mechsoul.graphc.cp

import otg.mechsoul.graphc.graph.Graph
import scala.collection.mutable.ListBuffer

class ConstraintEngine(val graph: Graph) {

  val NO_COLOR = -1

  val colors: Array[Int] = Array.fill(graph.nodeCount)(NO_COLOR)
  val vertexDomains: Array[List[Int]] = (0 until graph.nodeCount).map(vertex => List(0, 1, 2, 3)).toArray
  val processedNodes: List[Int] = Nil

  def checkEdges(): Boolean = {
    graph.edges.forall(edge => {
      checkColors(color(edge.from), color(edge.to))
    })
  }

  def checkEdges(vertex: Int): Boolean = {
    graph.incident(vertex).forall(v => checkColors(color(vertex), color(v)))
  }

  private def checkColors(color1: Int, color2: Int): Boolean = {
    color1 == NO_COLOR || color2 == NO_COLOR || color1 != color2
  }

  def checkDomains(vertex: Int): Boolean = !vertexDomains(vertex).isEmpty
  def checkDomains(): Boolean = (0 until graph.nodeCount).forall(checkDomains)

  def color(vertex: Int): Int = colors(vertex)
  def setColor(vertex: Int, color: Int) = colors(vertex) = color

  def choice(vertex: Int, color: Int): Boolean = {
    val setColor = new SetVertexColor(this, vertex, color)
    setColor.execute()
  }

}

abstract class Instruction(val cp: ConstraintEngine) {
  val executed: ListBuffer[Instruction] = new ListBuffer[Instruction]()

  def execute(): Boolean = {
    println("commit: " + this)
    //executed.append(this)
    val feasible = doExecute()

    val con = feasible && consequences()
    if (!con) {
      rollback()
    }
    con
  }

  def rollback(): Unit = {
    println("rollback: " + this)
    //rollback in reverse order
    executed.toList.reverse.foreach(i => i.rollback())
    doRollback()
  }

  protected def consequences(): Boolean
  protected def doExecute(): Boolean
  protected def doRollback(): Unit

}

class SetVertexColor(cp: ConstraintEngine, vertex: Int, color: Int) extends Instruction(cp) {

  val prevColor = cp.color(vertex)
  val prevDomains = cp.vertexDomains(vertex)

  override def doExecute(): Boolean = {
    cp.setColor(vertex, color);
    cp.vertexDomains(vertex) = Nil
    cp.checkEdges(vertex);
  }

  override def doRollback(): Unit = {
    cp.setColor(vertex, prevColor);
    cp.vertexDomains(vertex) = prevDomains
  }

  override def consequences(): Boolean = {
    cp.graph.incident(vertex).filter(v => cp.color(v) == cp.NO_COLOR).forall(incVertex => {
      val removeDomain = new RemoveVertexDomain(cp, incVertex, color)
      removeDomain.execute()
    })
  }

  override def toString = s"$vertex set color $color"
}

class RemoveVertexDomain(cp: ConstraintEngine, vertex: Int, color: Int) extends Instruction(cp) {

  override def doExecute(): Boolean = {
    cp.vertexDomains(vertex) = cp.vertexDomains(vertex).filterNot(_ == color)
    !cp.vertexDomains(vertex).isEmpty
  }

  override def doRollback(): Unit = {
    cp.vertexDomains(vertex) = color :: cp.vertexDomains(vertex)
  }

  override def consequences(): Boolean = {
    val vertexColor = cp.color(vertex)
    val domains = cp.vertexDomains(vertex)

    if (vertexColor == cp.NO_COLOR) {
      domains.size match {
        case 0 => false
        case 1 => new SetVertexColor(cp, vertex, domains.head).execute()
        case _ => true
      }
    } else { true }
  }

  override def toString = s"$vertex remove $color from domain ${cp.vertexDomains(vertex).mkString("(", ",", ")")}"

}