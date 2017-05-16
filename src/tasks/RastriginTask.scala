package tasks

import algo.SearchAlgo

/**
  * Created by k.neyman on 27.04.2017.
  */
class RastriginTask(size: Int, isLogged: Boolean = false) extends Task(size, isLogged) {
  private val f0 = -330

  override val realOptimum: Double = -1 * f0

  private def fi(x: Double): Double = x * x - 10 * math.cos(2 * math.Pi * x) + 10
  val f = new SearchAlgo.Function(list => -1 * (list.toStream.map(fi).sum + f0))
  override val realOptimumXs: List[Double] = (for { _ <- 0 until size } yield 0.0).toList
}
