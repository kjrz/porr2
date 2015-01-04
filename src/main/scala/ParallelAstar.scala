import akka.actor._
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
  implicit val timeout = Timeout(5 seconds)
  implicit val ec = ExecutionContext.Implicits.global
}

case object StartNode

case object NextNode

case class ProposePrev(cost: Long, prev: Int)

case object NodeDone

object SearchNode {
  def props(label: Int) = Props(new SearchNode(label))
}

class SearchNode(val label: Int) extends Actor with ActorLogging {

  import AstarAlgorithm._
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
    case StartNode =>
      setStart()
      sender ! NodeDone
    case NextNode =>
      proposePrev()
      sender ! NodeDone
    case ProposePrev(c, p) =>
      considerPrev(c, p)
      sender ! NodeDone
  }

  def setStart() {
    cost = Some(0L)
    open ! OpenNode(label, 0L)
  }

  def proposePrev() {
    log.debug("I'm next")
    val costValue = cost match {
      case Some(c) => c
      case None => throw new IllegalStateException("Node with no cost set can't go next")
    }
    val f = Future.traverse(neighbours) { n => n ? ProposePrev(costValue, label)}
    Await.result(f, atMost)
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
      log.debug("set new prev: " + possiblePrev)
      prev = Some(possiblePrev)
      cost = Some(possibleCost)
      open ! OpenNode(label, possibleCost)
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

case class OpenNode(label: Int, cost: Long)

case object Dequeue

case object NoNewNodes

class Open extends Actor {
  var queue = mutable.PriorityQueue[OpenNode]()(Ordering.by { n: OpenNode => n.cost}.reverse)

  def receive = {
    case n: OpenNode =>
      put(n)
    case Dequeue =>
      if (queue.isEmpty) sender ! NoNewNodes
      sender ! queue.dequeue
  }

  def put(node: OpenNode) = {
    queue = queue.filterNot(n => n.label == node.label)
    queue += node
  }
}

case object RunAlgorithm

object AstarAlgorithm {
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
  val start = 1
  val goal = 9

  val system = ActorSystem("astar")
  val nodes = system.actorOf(Props[Nodes], "nodes")
  val open = system.actorOf(Props[Open], "open")
}

class AstarAlgorithm extends Actor with ActorLogging {

  import AstarAlgorithm._
  import Implicit._

  override def preStart() {
    val a = getNode(start)
    val f = a ? StartNode
    Await.result(f, atMost)
  }

  def receive = {
    case RunAlgorithm | NodeDone =>
      open ! Dequeue
    case OpenNode(`goal`, _) =>
      log.info("done")
      system.shutdown()
    case NoNewNodes =>
      log.info("no luck with that")
      system.shutdown()
    case OpenNode(n, _) =>
      val a = getNode(n)
      a ! NextNode
  }

  def getNode(i: Int): ActorRef = {
    val f = nodes ? GetNode(i)
    Await.result(f, atMost).asInstanceOf[ReturnNode].actor
  }
}

object ParallelAstar extends App {

  import AstarAlgorithm.system

  val a = system.actorOf(Props[AstarAlgorithm], "algorithm")
  a ! RunAlgorithm
}

// TODO: logging
