import tasks.Task

/**
  * Created by k.neyman on 15.05.2017.
  */
class Analyzer(val size: Int, val taskCreation: (Int) => Task) extends AnalyzerUtils {
  val accuracyResult = 1e-3

  val task: () => Task = () => taskCreation(size)
  private val testAmount = 1e2.toInt

  private case class Result(optimum: Double, xs: List[Double], fCalculated: Int, iterationNum: Int)

  private val resultsTupled = (
    for { i <- (0 until testAmount) } yield {
      if (i % 25 == 0) println(s"size = $size, i = $i")
      val task1: Task = task()
      Result(task1.optimum, task1.xs, task1.f.amount, task1.iterationNum)
    }).toStream

  val results = resultsTupled.map { _.optimum }
  val resultXs = resultsTupled.map { _.xs }

  println(s"Size = $size finished")

  private val optimum: Double = task().realOptimum
  private val optimumXs: List[Double] = task().realOptimumXs

  val percentage = resultsTupled.map { v => math.abs(v.optimum - optimum) }.count(_ < accuracyResult).toDouble / resultsTupled.size
  val funcCalcultaedSqDeviation = resultsTupled.map { _.fCalculated.toDouble }.squareDeviation  // 8) среднее число испытаний
  val funcCalculatedTime = resultsTupled.map { _.fCalculated.toDouble }.average  // 7) среднее число испытаний
  val iterationAverage: Double = resultsTupled.map { _.iterationNum.toDouble }.average  // 6) среднее число итераций
  val averageXsEuclidDeviation: Double = resultXs.map(v => (v - optimumXs).euclidLen).average // 5) среднее значение лучшего Х
  val best: Double = results.max                               // 4) лучшее значение функции
  val bestXsEuclidDeviation: Double = resultXs.map(v => (v - optimumXs).euclidLen).min // 3) значение лучшего Х
  val sqDeviation: Double = results.squareDeviation   // 2) Среднеквадратичное
  val averageBest: Double = results.average           // 1) среднее лучшее значение

  val averageError: Double = math.abs(averageBest - optimum)
  val error: Double = math.abs(best - optimum)

  override def toString: String = {
    s"max = $best\n" +
      s"square deviation = $sqDeviation\n" +
      s"average = $averageBest\n"
  }

  implicit class AlgebraVector[B](list: Iterable[B])(implicit numeric: Numeric[B]) {
    def -(l2: Iterable[B]): Iterable[Double] = (list zip l2).map { case(l, r) => l - r }
    def euclidLen: Double = math.sqrt(list.map(math.pow(_, 2)).sum)
  }
}
