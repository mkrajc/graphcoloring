package otg.mechsoul.graphc.cp

import scala.collection.mutable.ListBuffer
import otg.mechsoul.graphc.graph.Graph
import otg.mechsoul.graphc.Choice

class ConstraintEngine(val graph: Graph) {

  val NO_COLOR = -1
  val MAX_COLORS = 4
  val LIMIT_COLOR = MAX_COLORS - 1
  val COLORS = List(1, 2, 3, 4)

  val colors: Array[Int] = Array.fill(graph.nodeCount)(NO_COLOR)
  val vertexForbiddenDomains: Array[List[Int]] = Array.fill(graph.nodeCount)(Nil)

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

  def checkDomain(vertex: Int): Boolean = vertexForbiddenDomains(vertex).size < MAX_COLORS
  def checkDomain(vertex: Int, color: Int): Boolean = !vertexForbiddenDomains(vertex).contains(color)
  def checkDomains(): Boolean = (0 until graph.nodeCount).forall(checkDomain)

  def checkColor(vertex: Int): Boolean = color(vertex) == NO_COLOR

  def color(vertex: Int): Int = colors(vertex)
  def setColor(vertex: Int, color: Int) = colors(vertex) = color

  def applyChoice(choice: Choice): Result = {
    val setColor = new SetVertexColor(this, choice.vertex, choice.color)
    val success = setColor.execute()
    new Result(success, List(setColor))
  }

  def giveNextChoices(): List[Choice] = {
    val start = colors.forall(_ == NO_COLOR)
    val result = if (start) {
      // give highest degree node with one color
      val maximalDeg = graph.deg.head
      List(Choice(maximalDeg._2, 0))
    } else {
      Nil
    }
    
    result
  }
  def isSolved(): Boolean = false

}

class Result(val success: Boolean, instructions: List[Instruction]) {
  def rollback() {
    instructions.foreach { i => i.rollback() }
  }
}

abstract class Instruction(val cp: ConstraintEngine) {
  val executed: ListBuffer[Instruction] = new ListBuffer[Instruction]()

  def execute(): Boolean = {
    println("commit: " + this)
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
  val prevDomains = cp.vertexForbiddenDomains(vertex)

  override def doExecute(): Boolean = {
    if (cp.checkColor(vertex) && cp.checkDomain(vertex, color)) {
      cp.setColor(vertex, color);
      cp.vertexForbiddenDomains(vertex) = Nil
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
      cp.vertexForbiddenDomains(vertex) = color :: cp.vertexForbiddenDomains(vertex)
      cp.checkDomain(vertex)
    } else false
  }

  override def doRollback(): Unit = {
    cp.vertexForbiddenDomains(vertex) = cp.vertexForbiddenDomains(vertex).tail
  }

  override def consequences(): Boolean = {
    val vertexColor = cp.color(vertex)
    val domains = cp.vertexForbiddenDomains(vertex)

    if (vertexColor == cp.NO_COLOR) {
      domains.size match {
        case i if i < cp.LIMIT_COLOR => true
        case cp.LIMIT_COLOR =>
          val onlyAllowed = cp.COLORS.diff(domains).head
          val setColor = new SetVertexColor(cp, vertex, onlyAllowed)
          executed.append(setColor)
          setColor.execute()
        case _ => false
      }
    } else { true }
  }

  override def toString = s"$vertex forbid $color domain ${cp.vertexForbiddenDomains(vertex).mkString("(", ",", ")")}"

}