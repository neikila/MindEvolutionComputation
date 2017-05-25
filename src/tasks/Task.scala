package tasks

import algo.{Group, Individual, MecAlgo, SearchAlgo}

/**
  * Created by k.neyman on 27.04.2017.
  */
abstract class Task (val name: String, val size: Int, val isLogged: Boolean = false) {
  implicit val f: SearchAlgo.Function
  protected implicit val bounds: List[(Double, Double)] = List.tabulate(size) { _ => (-1.0, 1.0) }
  protected implicit val distributions: List[Double] = List.tabulate(size) { _ => 0.01 }
  val realOptimum: Double
  val realOptimumXs: List[Double]

  var iterationNum = 0

  def execute() = {
    val algo: MecAlgo = new MecAlgo(f, isLogged)(distributions, bounds)
    val result = algo.solve()
    iterationNum = algo.iterationNum
    result
  }

  override def toString: String = {
    println("Result group:")
    val resultGroup = execute()
    resultGroup.individuals.foreach(println)
    println(s"Best group: ${resultGroup.leader}")
    println(s"Optimum: ${resultGroup.leader.count}")
    ""
  }

  private lazy val leader: Individual = execute().leader

  lazy val optimum = leader.count
  lazy val xs = leader.xs
}
