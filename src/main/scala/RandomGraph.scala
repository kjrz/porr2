import scala.util.Random
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WDiEdge

object RandomGraph {
  val tinyGraph = Graph(
    1 ~> 2 % 1, 1 ~> 3 % 2,
    2 ~> 5 % 3, 2 ~> 7 % 4,
    3 ~> 2 % 1, 3 ~> 4 % 4,
    4 ~> 6 % 2,
    5 ~> 3 % 3, 5 ~> 7 % 1,
    6 ~> 5 % 5, 6 ~> 8 % 2, 6 ~> 9 % 1,
    7 ~> 8 % 3,
    8 ~> 5 % 1, 8 ~> 9 % 6
  )

  val dice = new Random()

  def randomGraph(order: Int, edgeRange: Int, maxWeight: Int): Graph[Int, WDiEdge] = {

    def fromNode(node: Int, range: Int): Seq[WDiEdge[Int]] = {
      if (node % 50 == 0) print(node + "/" + order + "\n")
      else print(".")
      for {
        n <- node + 1 to node + range
        if dice.nextBoolean()
      } yield node ~> n % (dice.nextInt(maxWeight) + 1)
    }

    def extend(i: Int, g: Graph[Int, WDiEdge]): Graph[Int, WDiEdge] =
      if (i < order - edgeRange) extend(i + 1, g ++ fromNode(i, edgeRange))
      else if (i < order - 1) extend(i + 1, g ++ fromNode(i, order - 1 - i))
      else g

    println("Creating graph")
    print(".")
    val entranceAndExit = Graph(1 ~> 2 % 1, (order - 1) ~> order % maxWeight)
    extend(2, entranceAndExit)
  }
}
