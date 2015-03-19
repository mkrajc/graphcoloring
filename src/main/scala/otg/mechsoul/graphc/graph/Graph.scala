package otg.mechsoul.graphc.graph

class Graph(val nodeCount: Int, val edges: Array[Edge]) {

  val degrees: Array[Int] = new Array[Int](nodeCount)
  val incidentVerteces: Array[List[Int]] = Array.fill[List[Int]](nodeCount)(Nil)

  edges.foreach(edge => {
    degrees(edge.from) = degrees(edge.from) + 1
    degrees(edge.to) = degrees(edge.to) + 1
    incidentVerteces(edge.from) = edge.to :: incidentVerteces(edge.from);
    incidentVerteces(edge.to) = edge.from :: incidentVerteces(edge.to);
  })

  def incident(vertex: Int): List[Int] = incidentVerteces(vertex)

  override def toString(): String = s"nc=$nodeCount edges=${edges.mkString(",")} deg=${degrees.mkString(",")}"

}

case class Edge(val from: Int, val to: Int) {
  override def toString(): String = s"($from, $to)"
}