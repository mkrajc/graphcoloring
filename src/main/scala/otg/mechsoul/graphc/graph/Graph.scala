package otg.mechsoul.graphc.graph

class Graph(val nodeCount: Int, val edges: Array[Edge]) {

  val degrees: Array[Int] = new Array[Int](nodeCount)

  edges.foreach(edge => {
    degrees(edge.from) = degrees(edge.from) + 1
    degrees(edge.to) = degrees(edge.to) + 1
  })

  override def toString(): String = s"nc=$nodeCount edges=${edges.mkString(",")} deg=${degrees.mkString(",")}"

}

case class Edge(val from: Int, val to: Int) {
  override def toString(): String = s"($from, $to)"
}