import java.util.concurrent.TimeUnit

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._

case object SerialLetter

object SearchNode {
  def props(label: Int) = Props(new SearchNode(label))
}

class SearchNode(label: Int) extends Actor {
  def receive = {
    case SerialLetter => println("search node " + label)
  }
}

case class GetNode(label: Int)

case class ReturnNode(node: ActorRef)

class Nodes extends Actor {
  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  def receive = {
    case GetNode(n) =>
      val node: ActorRef = getOrCreate(n)
      sender() ! ReturnNode(node)
  }

  def getOrCreate(n: Int): ActorRef = {
    val f = context.actorSelection(n.toString).resolveOne()
    val node = try {
      Await.result(f, timeout.duration)
    } catch {
      case _: ActorNotFound => context.actorOf(SearchNode.props(n), n.toString)
    }
    node
  }
}

object ParallelAstar extends App {

  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

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
  val nodes = system.actorOf(Props[Nodes], "node")

  val f = nodes ? GetNode(1)
  val ans = Await.result(f, timeout.duration).asInstanceOf[ReturnNode]
  ans.node ! SerialLetter
  println(ans.node.path)

  system.shutdown()
}
