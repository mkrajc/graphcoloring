package otg.mechsoul.graphc.cp

import otg.mechsoul.graphc.Choice
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

abstract class ChoiceProvider(val cp: ConstraintEngine) {
  private var results: List[Result] = Nil

  def popLastResult: Result = {
    val head = results.head
    results = results.tail
    head
  }

  def resultsSize(): Int = results.length

  def next(ch: Choice): Choice = {
    val result = cp.applyChoice(ch)

    if (result.success) {
      results = result :: results
      success(ch)
    } else {
      failure(ch)
    }
  }

  def success(ch: Choice): Choice = {
    if (results.length < cp.graph.nodeCount) {
      val next = nextVertex(ch.vertex)
      new Choice(next, nextAvailableColor(next, 0))
    } else {
      ch
    }
  }

  def failure(ch: Choice): Choice = {
    new Choice(ch.vertex, nextAvailableColor(ch.vertex, ch.color + 1))
  }

  def firstVertex: Int
  def nextVertex(v: Int): Int

  private def nextAvailableColor(vertex: Int, fromColor: Int): Int = {
    val fc = cp.vertexForbiddenDomains(vertex)
    if (fc.isEmpty) { fromColor } else {
      if (fc.contains(fromColor)) {
        nextAvailableColor(vertex, fromColor + 1)
      } else {
        fromColor
      }
    }
  }

}

class GreedySmartChoicer(cp: ConstraintEngine) extends ChoiceProvider(cp) {

  def firstVertex = cp.graph.deg(0)._2

  def nextVertex(v: Int): Int = {
    var currentIndex = cp.graph.deg.indexWhere(p => p._2 == v) + 1
    cp.graph.deg(currentIndex)._2
  }
}

class GreedyChoicer(cp: ConstraintEngine) extends ChoiceProvider(cp) {
  def firstVertex = 0
  def nextVertex(v: Int): Int = v + 1
}
