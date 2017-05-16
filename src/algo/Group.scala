package algo

import algo.Group.Bounds
import algo.SearchAlgo.{Function => Fun}
import generators.{ChooseDoubleGen, DoubleGen, Generator, ListBasedGen}

/**
  * Created by k.neyman on 29.03.2017.
  */
class Group(val individuals: List[Individual])
           (implicit val distrs: List[Double],
            val bounds: List[Bounds]) {
  def leader(implicit f: Fun): Individual = individuals.maxBy(_.count)

  def recreate(implicit f: Fun) = Group.create(leader, individuals.size - 1)

  override def toString: String = individuals.toString()
}

object Group {
  type Up = Double
  type Bottom = Double
  type Bounds = (Bottom, Up)

  private implicit val doubleGen: Generator[Double] = new DoubleGen

  def create(size: Int)(implicit distrs: List[Double], bounds: List[Bounds]): Group = {
    val individual = Individual(bounds
      .map { case ((bottom, up)) => new ChooseDoubleGen(bottom, up) }
      .map { _.generate })
    create(individual, size - 1)
  }

  private def adjustWithBounds(values: List[Double])(implicit bounds: List[Bounds]) = {
    (values zip bounds).map { case (value, (bottom, up)) => math.max(math.min(value, up), bottom) }
  }

  def create(leader: Individual, size: Int)(implicit distrs: List[Double], bounds: List[Bounds]): Group = {
    val individualGenerator = new ListBasedGen(leader.xs, distrs).map(adjustWithBounds).map(Individual)
    val newIndividuals: List[Individual] = leader ::
      List.tabulate(size) { _ => individualGenerator.generate }
    new Group(newIndividuals)
  }
}