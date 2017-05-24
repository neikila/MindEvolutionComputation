import tasks._

/**
  * Created by k.neyman on 29.03.2017.
  */
object Main extends AnalyzerUtils {

  private val size = 32

  val tasks: List[(Int) => Task] = ((s: Int) => new SphereTask(s)) ::
    ((s: Int) => new RastriginTask(s)) ::
    ((s: Int) => new RosenbrokeTask(s)) ::
    Nil

  def main(args: Array[String]): Unit = {
    createAnalyze()
  }

  def createAnalyze() {
    val analyzersPar = for {
      i <- (1 to 3).toParArray
      t <- tasks.take(1)
    } yield new Analyzer(math.pow(2, i).toInt, t)

    val analyzers = analyzersPar.toList

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
      "teta(m)" -> (_.funcCalcultaedSqDeviation)
    ).foreach { case (name, func) => printWithMessage(name, func) }
  }
}
