import scala.collection.mutable
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

object SerialAstar extends App {

  val start: Int = 1

  var graph: Graph[Int, WDiEdge] = RandomGraph.tinyGraph
  var goal = graph.order

  object Nodes {
    val dict = mutable.Map[Int, SearchNode]()

    def get(label: Int): SearchNode = dict.getOrElseUpdate(label, new SearchNode(label, graph))

    def clear() = dict.clear()
  }

  class SearchNode(val label: Int, val graph: Graph[Int, WDiEdge]) {
    var prev: Option[SearchNode] = None
    var cost: Option[Long] = None

    def talkToNeighbours() {
      graph.get(label).diSuccessors.foreach(
        n => Nodes.get(n).listenToNeighbour(this))
    }

    def listenToNeighbour(sender: SearchNode) {
      val newCost = {
        val senderCost: Long = sender.cost match {
          case Some(c) => c
        }
        val edgeCost: Long =
          graph.get(sender.label).findOutgoingTo(graph.get(label)) match {
            case Some(e) => e.weight
            case None => throw new IllegalStateException("No edge")
          }
        senderCost + edgeCost
      }

      (cost, prev) match {
        case (None, None) =>
          newPrev()
        case (Some(c), _) =>
          if (newCost < c) {
            newPrev()
          }
      }

      def newPrev() {
//        println("I am " + label + " and I listen")
//        println("New cost " + newCost)

        prev = Some(sender)
        cost = Some(newCost)
        Open.put(label)
      }
    }

    override val toString = label.toString
  }

  object Open {
    val queue = mutable.PriorityQueue[Int]()(Ordering.by { n: Int => Nodes.get(n).cost}.reverse)

    def put(label: Int) = if (!contains(label)) queue += label

    def dequeue(): SearchNode = Nodes.get(queue.dequeue())

    def contains(label: Int): Boolean = queue.exists(i => i == label)

    def empty(): Boolean = queue.isEmpty

    def clear() = queue.clear()
  }

  def search(next: SearchNode) {
    if (next.label == goal) {
      println("found")
    } else {
      next.talkToNeighbours()
      if (Open.empty()) {
        println("not found")
      } else {
        search(Open.dequeue())
      }
    }
  }

  def read(label: Int) {
    if (label == goal) {
      Nodes.get(label).cost match {
        case Some(c) => print("[" + c + "] ")
      }
    }
    print(label)
    if (label != start) {
      print("<-")
      Nodes.get(label).prev match {
        case Some(p) => read(p.label)
      }
    }
  }

  def reload(g: Graph[Int, WDiEdge]): Stats = {
    graph = g
    goal = graph.order

    Nodes.clear()
    Open.clear()

    Nodes.get(1).cost = Some(0)
    Open.put(1)

    val startTime = System.nanoTime()

    search(Open.dequeue())

    val endTime = System.nanoTime()

    Stats(endTime - startTime)
  }
}
