package algo

import tasks.AlgoSettings

/**
  * Created by k.neyman on 29.03.2017.
  */
class MecAlgo(val f: SearchAlgo.Function,
              val isLogged: Boolean = false)
             (implicit val distributions: List[Double],
              val bounds: List[(Double, Double)],
              val algoSettings: AlgoSettings) {

  implicit val func: SearchAlgo.Function = f

  assert(distributions.size == bounds.size)

  private val groupSize = algoSettings.groupSize
  private val groupAmount = algoSettings.groupAmount

  private val accuracy = 1e-6
  private val repeatTimeLimit = 30

  def solve(): Group = {
    val winners = create(groupAmount / 2)
    val losers = create(groupAmount - groupAmount / 2)
    solve(winners, losers)
  }

  var iterationNum: Int = -1
  var repeatTime: Int = -1

  @scala.annotation.tailrec
  private def solve(winners: List[Group], losers: List[Group]): Group = {
    iterationNum += 1
    val maxPrev = winners.head.leader.count
    val (newWin, newLose) = dissimilate(winners.map(_.recreate), losers.map(_.recreate))
    val result: Double = newWin.head.leader.count
    if (isLogged) println("%,f ".format(result))
    if (math.abs(maxPrev - result) > accuracy) {
      repeatTime = 0
    } else {
      repeatTime += 1
    }

    if (repeatTime < repeatTimeLimit) {
      solve(newWin, newLose)
    } else {
      newWin.head
    }
  }

  private def create(size: Int) = List.tabulate(size) { _ => Group.create(groupSize) } sortBy sortGroups

  private def dissimilate(winners: List[Group], losers: List[Group]): (List[Group], List[Group]) = {
    ((winners ::: losers).sortBy(sortGroups).take(groupAmount / 2), create(groupAmount - groupAmount / 2))
  }

  private val sortGroups: Group => Double = el => -el.leader.count
}
