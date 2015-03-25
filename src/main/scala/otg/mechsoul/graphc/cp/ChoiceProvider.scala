package otg.mechsoul.graphc.cp

import otg.mechsoul.graphc.Choice
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

abstract class ChoiceProvider(val cp: ConstraintEngine) {
  val currentChoices: ListBuffer[Choice] = new ListBuffer[Choice]()

  def next(ch: Choice): Choice = {
    val result = cp.applyChoice(ch)

    if (result.success) {
      success(ch)
    } else {
      failure(ch)
    }
  }

  def success(ch: Choice): Choice = {
    currentChoices += ch
    if (currentChoices.length < cp.graph.nodeCount) {
      val next = nextVertex(ch.vertex)
      new Choice(next, nextAvailableColor(next, 0))
    } else {
      null
    }
  }

  def failure(ch: Choice): Choice = {
    new Choice(ch.vertex, nextAvailableColor(ch.vertex, 0))
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
  private var currentIndex = 0

  def firstVertex = cp.graph.deg(currentIndex)._2
  
  def nextVertex(v: Int): Int = {
    currentIndex = currentIndex + 1
    cp.graph.deg(currentIndex)._2
  }
}

class GreedyChoicer(cp: ConstraintEngine) extends ChoiceProvider(cp) {
  def firstVertex = 0
  def nextVertex(v: Int): Int = v + 1
}
