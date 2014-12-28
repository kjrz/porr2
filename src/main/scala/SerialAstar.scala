import scala.collection.mutable
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

object SerialAstar extends App {

  val graph = Graph(
    1 ~> 2 % 1, 1 ~> 3 % 2,
    2 ~> 5 % 3, 2 ~> 7 % 4,
    3 ~> 2 % 1, 3 ~> 4 % 4,
    4 ~> 6 % 2,
    5 ~> 3 % 3, 5 ~> 7 % 1,
    6 ~> 5 % 5, 6 ~> 8 % 2, 6 ~> 9 % 1,
    7 ~> 8 % 3,
    8 ~> 5 % 1, 8 ~> 9 % 6
  )

  object Nodes {
    val dict = mutable.Map[Int, SearchNode]()

    def get(label: Int): SearchNode = dict.getOrElseUpdate(label, new SearchNode(label))
  }

  class SearchNode(val label: Int) {
    val node = graph.get(label)
    var prev: Option[SearchNode] = None
    var cost: Option[Long] = None

    def talkToNeighbours() {
      println("I am " + label + " and I talk")

      graph.get(label).diSuccessors.foreach(
        n => Nodes.get(n).listenToNeighbour(this))
    }

    def listenToNeighbour(sender: SearchNode) {
      val newCost = {
        val senderCost: Long = sender.cost match {
          case Some(c) => c
        }
        val edgeCost: Long = sender.node.findOutgoingTo(node) match {
          case Some(e) => e.weight
        }
        senderCost + edgeCost
      }

      def newPrev() {
        println("I am " + label + " and I listen")
        println("New cost " + newCost)

        prev = Some(sender)
        cost = Some(newCost)
        Open.put(label)
      }
      
      (cost, prev) match {
        case (None, None) =>
          newPrev()
        case (Some(c), _) =>
          if (newCost < c) {
            newPrev()
          }
      }
    }

    override val toString = label.toString
  }

  object Open {
    var queue = mutable.PriorityQueue[Int]()(Ordering.by { n: Int => Nodes.get(n).cost}.reverse)

    def put(label: Int) = if(!contains(label)) queue += label

    def dequeue(): SearchNode = Nodes.get(queue.dequeue())

    def contains(label: Int): Boolean = queue.exists(i => i == label)

    def remove(label: Int) = queue = queue.filterNot(i => i == label)

    def empty(): Boolean = queue.isEmpty
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

  val start = 1
  val goal = 9

  Nodes.get(start).cost = Some(0)
  Open.put(start)

  search(Open.dequeue())
  read(goal)
}
