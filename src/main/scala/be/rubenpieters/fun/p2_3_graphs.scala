package be.rubenpieters.fun

object p2_3_graphs extends App {
  sealed trait Graph[G] {
    type Vertex
    type Edge
    def src(e: Edge): Vertex
    def tgt(e: Edge): Vertex
    def outEdges(g: G, v: Vertex): List[Edge]
  }

  object Graph {
    type Aux[G, V, E] = Graph[G] { type Vertex = V ; type Edge = E }
    def apply[G](implicit ev: Graph[G]): Aux[G, ev.Vertex, ev.Edge] = ev
  }

  case class MkEdge1(v1: Int, v2: Int)
  case class G1(g1: List[MkEdge1])

  implicit def g1Graph[G]: Graph.Aux[G1, Int, MkEdge1] = new Graph[G1] {
    override type Vertex = Int
    override type Edge = MkEdge1

    override def outEdges(g: G1, v: Int): List[MkEdge1] = g.g1.filter(isSource(v, _))
    private def isSource(v: Int, e: MkEdge1): Boolean = e.v1 == v

    override def tgt(e: MkEdge1): Int = e.v2
    override def src(e: MkEdge1): Int = e.v1
  }

  val test = G1(List(MkEdge1(1, 2), MkEdge1(2, 3), MkEdge1(3, 1)))

  println(Graph[G1].outEdges(test, 1))

  def neighbours[G, V, E](g: G, v: V)(implicit graphG: Graph.Aux[G, V, E]): List[V] =
    graphG.outEdges(g, v).map(graphG.tgt)

  println(neighbours(test, 1))

  // attempt at injective graph
  // -> each instance has its own definition for Edge

  case class GI(g1: List[InjectiveGraph.Edge])

  // this signature doesn't seem to compile although we would want this one
  // implicit object InjectiveGraph extends Graph.Aux[GI, Int, InjectiveGraph.Edge] {
  implicit object InjectiveGraph extends Graph[GI] {
    case class MkEdgeI(v1: Int, v2: Int)
    override type Vertex = Int
    override type Edge = MkEdgeI

    override def outEdges(g: GI, v: Int): List[MkEdgeI] = g.g1.filter(x => isSource(v, x))
    private def isSource(v: Int, e: MkEdgeI): Boolean = e.v1 == v

    override def tgt(e: MkEdgeI): Int = e.v2
    override def src(e: MkEdgeI): Int = e.v1
  }

  val testI = GI(List(InjectiveGraph.MkEdgeI(1, 2), InjectiveGraph.MkEdgeI(2, 3), InjectiveGraph.MkEdgeI(3, 1)))

  // doesn't work because we can't have the Aux signature
  // val result: InjectiveGraph.MkEdgeI = Graph[GI].outEdges(testI, 1)
  println(neighbours(testI, 1))
}
