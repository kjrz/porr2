
object AstarJudge extends App {
  val g = RandomGraph.randomGraph(order = 100, edgeRange = 10, maxWeight = 10)

  val timeS = SerialAstar.reload(g)
  val timeP = AstarAlgorithm.reload(g)

  val milli = 1000000
  println("s. " + timeS.timeElapsed / milli)
  println("p. " + timeP.timeElapsed / milli)
  println("r. " + timeP.timeElapsed / timeS.timeElapsed)
}
