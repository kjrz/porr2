import akka.actor._

//import akka.pattern.ask

import akka.pattern.ask
import akka.util.Timeout

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

object Implicit {
  val atMost = 5 seconds
  implicit val timeout = Timeout(atMost)
  implicit val ec = ExecutionContext.Implicits.global
}

object Commons {
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
  val system = ActorSystem("astar")
  val nodes = system.actorOf(Props[Nodes], "nodes")
}

object SearchNode {
  def props(label: Int) = Props(new SearchNode(label))
}

case object Start

case object YouGoNext

case class ProposePrev(cost: Long, prev: Int)

class SearchNode(val label: Int) extends Actor {

  import Commons._
  import Implicit._

  var cost: Option[Long] = None
  var prev: Option[Int] = None

  lazy val neighbours = {
    val indices = graph.get(label).diSuccessors
    val f = Future.traverse(indices) { i => nodes ? GetNode(i)}
    val r = Await.result(f, atMost).asInstanceOf[Iterable[ReturnNode]]
    r.map(s => s.actor)
  }

  def receive = {
    case Start =>
      cost = Some(0L)
    case YouGoNext =>
      proposePrev()
    case ProposePrev(c, p) =>
      considerPrev(c, p)
  }

  def proposePrev() {
    println("(" + label + ") I'm next")
    cost match {
      case Some(c) => neighbours.foreach(n => n ! ProposePrev(c, label))
      case None => throw new IllegalStateException("Node with no cost set can't go next")
    }
  }

  def considerPrev(possiblePrevCost: Long, possiblePrev: Int) {
    val possibleCost: Long = {
      val possiblePrevNode = graph.get(possiblePrev)
      val thisNode = graph.get(label)
      val edgeCost: Long = possiblePrevNode.findOutgoingTo(thisNode) match {
        case Some(e) => e.weight
        case None => throw new IllegalStateException("No edge")
      }
      possiblePrevCost + edgeCost
    }

    (cost, prev) match {
      case (None, None) =>
        setNewPrev()
      case (Some(c), _) =>
        if (possibleCost < c) {
          setNewPrev()
        }
      case _ =>
        throw new IllegalStateException()
    }

    def setNewPrev() {
      println("(" + label + ") set new prev: " + possiblePrev)
      prev = Some(possiblePrev)
      cost = Some(possibleCost)
      // TODO:
      // open ! OpenNode(label, possibleCost)
    }
  }
}

case class GetNode(label: Int)

case class ReturnNode(actor: ActorRef)

class Nodes extends Actor {

  import Implicit._

  def receive = {
    case GetNode(n) =>
      sender() ! ReturnNode(getOrCreate(n))
  }

  def getOrCreate(n: Int): ActorRef = {
    val f = context.actorSelection(n.toString).resolveOne()
    try {
      Await.result(f, atMost)
    } catch {
      case _: ActorNotFound => context.actorOf(SearchNode.props(n), n.toString)
    }
  }
}

case object Dequeue

case object NoNewNodes

case class OpenNode(label: Int, cost: Long)

class Open extends Actor {
  var queue = mutable.PriorityQueue[OpenNode]()(Ordering.by { n: OpenNode => n.cost}.reverse)

  def receive = {
    case n: OpenNode =>
      println("a")
      put(n)
    case Dequeue =>
      println("n")
      if (queue.isEmpty) sender ! NoNewNodes
      sender ! queue.dequeue
    case _ =>
      println("yyyy")
  }

  def put(node: OpenNode) = {
    queue = queue.filterNot(n => n.label == node.label)
    queue += node
  }
}

object ParallelAstar extends App {

  import Commons._

//  var f = nodes ? GetNode(1)
//  var a = Await.result(f, atMost).asInstanceOf[ReturnNode].actor
//  a ! Start
//  a ! YouGoNext

//  Thread.sleep(2000)

  val open = system.actorOf(Props[Nodes], "open")

  open ! OpenNode(1, 0L)
  // TODO: no reaction?

//  f = open ? Dequeue
//  var b = Await.result(f, atMost).asInstanceOf[OpenNode]
//  println(b)

  Thread.sleep(2000)
  system.shutdown()
}
