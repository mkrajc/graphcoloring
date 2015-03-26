package otg.mechsoul.graphc.cp

import scala.collection.mutable.ListBuffer
import otg.mechsoul.graphc.graph.Graph
import otg.mechsoul.graphc.Choice

class ConstraintEngine(val graph: Graph) {

  val NO_COLOR = -1 

  val colors: Array[Int] = Array.fill(graph.nodeCount)(NO_COLOR)
  val vertexForbiddenDomains: Array[Set[Int]] = Array.fill(graph.nodeCount)(Set())

  def checkFeasibility(): Boolean = {
    graph.edges.forall(edge => {
      val c1 = color(edge.from)
      val c2 = color(edge.to)
      c1 != NO_COLOR && c2 != NO_COLOR && checkColors(c1,c2)
    })
  }
  
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

  def checkDomain(vertex: Int, color: Int): Boolean = !vertexForbiddenDomains(vertex).contains(color)

  def checkColor(vertex: Int): Boolean = color(vertex) == NO_COLOR

  def color(vertex: Int): Int = colors(vertex)
  def setColor(vertex: Int, color: Int) = colors(vertex) = color
  
  def colorsCount = colors.filterNot(_ == NO_COLOR).distinct.length

  def applyChoice(choice: Choice): Result = {
    val setColor = new SetVertexColor(this, choice.vertex, choice.color)
    val success = setColor.execute()
    new Result(success, List(setColor), choice)
  }
  
  

}

class Result(val success: Boolean, instructions: List[Instruction], val choice: Choice) {
  def rollback() {
    instructions.foreach { i => i.rollback() }
  }
  
  override def toString = s"result $success, $choice"
}

abstract class Instruction(val cp: ConstraintEngine) {
  val executed: ListBuffer[Instruction] = new ListBuffer[Instruction]()

  def execute(): Boolean = {
    // println("commit: " + this)
    val feasible = doExecute()

    val con = feasible && consequences()
    if (!con) {
      rollback()
    }
    con
  }

  def rollback(): Unit = {
     // println("rollback: " + this)
    // rollback in reverse order
    executed.toList.reverse.foreach(i => i.rollback())
    doRollback()
  }

  protected def consequences(): Boolean
  protected def doExecute(): Boolean
  protected def doRollback(): Unit

}

class SetVertexColor(cp: ConstraintEngine, vertex: Int, color: Int) extends Instruction(cp) {

  val prevColor = cp.color(vertex)
  val prevDomains = cp.vertexForbiddenDomains(vertex)

  override def doExecute(): Boolean = {
    if (cp.checkColor(vertex) && cp.checkDomain(vertex, color)) {
      cp.setColor(vertex, color);
      cp.vertexForbiddenDomains(vertex) = Set()
      cp.checkEdges(vertex);
    } else false
  }

  override def doRollback(): Unit = {
    cp.setColor(vertex, prevColor);
    cp.vertexForbiddenDomains(vertex) = prevDomains
  }

  override def consequences(): Boolean = {
    cp.graph.incident(vertex).filter(cp.checkColor).forall(incVertex => {
      val removeDomain = new ForbidVertexDomain(cp, incVertex, color)
      executed.append(removeDomain)
      removeDomain.execute()
    })
  }

  override def toString = s"$vertex set color $color"
}

class ForbidVertexDomain(cp: ConstraintEngine, vertex: Int, color: Int) extends Instruction(cp) {

  override def doExecute(): Boolean = {
    if (cp.checkColor(vertex)) {
      cp.vertexForbiddenDomains(vertex) = cp.vertexForbiddenDomains(vertex) + color
      true
    } else false
  }

  override def doRollback(): Unit = {
    cp.vertexForbiddenDomains(vertex) = cp.vertexForbiddenDomains(vertex) - color
  }

  override def consequences(): Boolean = true
  
  override def toString = s"$vertex forbid $color domain ${cp.vertexForbiddenDomains(vertex).mkString("(", ",", ")")}"

}