package otg.mechsoul.graphc.graph

import scala.util.Sorting

class Graph(val nodeCount: Int, val edges: Array[Edge]) {

  val incidentVerteces: Array[List[Int]] = Array.fill[List[Int]](nodeCount)(Nil)
  val deg: Array[(Int, Int)] = createOrderedDegrees()

  private def createOrderedDegrees(): Array[(Int, Int)] = {
    val degrees: Array[Int] = new Array[Int](nodeCount)
    
    edges.foreach(edge => {
      degrees(edge.from) = degrees(edge.from) + 1
      degrees(edge.to) = degrees(edge.to) + 1

      incidentVerteces(edge.from) = edge.to :: incidentVerteces(edge.from);
      incidentVerteces(edge.to) = edge.from :: incidentVerteces(edge.to);
    })

    val d = degrees.zipWithIndex
    Sorting.stableSort(d, (e1: Tuple2[Int, Int], e2: Tuple2[Int, Int]) => e1._1 > e2._1)
    d
  }

  def incident(vertex: Int): List[Int] = incidentVerteces(vertex)

  override def toString(): String = s"nc=$nodeCount edges=${edges.mkString(",")} deg=${deg.mkString(",")}"

}

case class Edge(val from: Int, val to: Int) {
  override def toString(): String = s"($from, $to)"
}