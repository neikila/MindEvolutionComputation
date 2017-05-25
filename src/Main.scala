import tasks._

/**
  * Created by k.neyman on 29.03.2017.
  */
object Main extends AnalyzerUtils {

  val algoSettings = AlgoSettings(0.01, 12, 20)

  val tasks: List[(Int) => Task] = ((s: Int) => new SphereTask(s, algoSettings)) ::
    ((s: Int) => new RastriginTask(s, algoSettings)) ::
    ((s: Int) => new RosenbrokeTask(s, algoSettings)) ::
    Nil

  def main(args: Array[String]): Unit = {
//    new SphereTask(32, true).execute()
    createAnalyze()
  }

  def createAnalyze() {
    val analyzersPar = for {
      i <- (3 until 4).toParArray
      t <- tasks.slice(2, 3)
    } yield new Analyzer(math.pow(2, i).toInt, t)

    val analyzers = analyzersPar.toList
    println(analyzers.head.task().name)
    printAllInfo(analyzers)
  }

  def printAllInfo(analyzers: List[Analyzer]): Unit = {
    def printParam(f: Analyzer => Double): Unit = {
      analyzers.map(f).map("%,.3f; ".format(_)).foreach(print)
    }

    def printWithMessage(str: String, f: Analyzer => Double): Unit = {
      print(str + "; ")
      printParam(f)
      println()
    }

    List[(String, Analyzer => Double)](
      "|X|" -> (_.size.toDouble),
      "f*" -> (_.best),
      "f**" -> (_.averageBest),
      "x*" -> (_.bestXsEuclidDeviation),
      "x**" -> (_.averageXsEuclidDeviation),
      "t av" -> (_.iterationAverage),
      "m" -> (_.funcCalculatedTime),
      "teta(f*)" -> (_.sqDeviation),
      "teta(m)" -> (_.funcCalcultaedSqDeviation),
      "P" -> (_.percentage)
    ).foreach { case (name, func) => printWithMessage(name, func) }
  }
}
