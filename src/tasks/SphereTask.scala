package tasks

import algo.SearchAlgo

/**
  * Created by k.neyman on 27.04.2017.
  */
class SphereTask(size: Int, isLogged: Boolean = false) extends Task(size, isLogged) {
  private val b0 = 10

  override val realOptimum: Double = -b0
  val f = new SearchAlgo.Function(list => -(b0 + list.toStream.map(x => x * x).sum))
  override val realOptimumXs: List[Double] = (for { _ <- 0 until size } yield 0.0).toList
}
