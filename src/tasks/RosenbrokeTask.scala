package tasks

import algo.SearchAlgo

/**
  * Created by k.neyman on 27.04.2017.
  */
class RosenbrokeTask(size: Int, isLogged: Boolean = false) extends Task(size, isLogged) {
  private val f0 = 390

  override val realOptimum: Double = -1 * f0

  private def fi(xi: Double, xi1: Double): Double =
    100 * math.pow(xi * xi - xi1, 2) + math.pow(xi - 1, 2)

  val f = new SearchAlgo.Function(list =>
    ((list zip list.drop(1)).map { case (xi, xi1) => fi(xi, xi1) }.sum + f0) * -1)

  override val realOptimumXs: List[Double] = (for { _ <- 0 until size } yield 1.0).toList
}
