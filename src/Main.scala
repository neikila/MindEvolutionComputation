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
//    createAnalyze()
//    analyzeGroupSize()
    analyzeGroupAmount()
  }

  def analyzeSigma(): Unit = {
    val analyzersPar = for {
      pow <- 0 until 6
    } yield {
      val sigma: Double = 2.toDouble / math.pow(4, pow)
      val newSet: AlgoSettings = algoSettings.copy(sigma = sigma)
      val taskCreation = (s: Int) => new RosenbrokeTask(s, newSet)
      new Analyzer(math.pow(2, 3).toInt, taskCreation)
    }
    val analyzers = analyzersPar.toList
    println(analyzers.head.task().name)
    print("sigma; ")
    printParam(analyzers, _.task().algoSettings.sigma)
    println()
    printAllInfo(analyzers)
  }

  def analyzeGroupSize(): Unit = {
    val analyzersPar = for {
      pow <- 4 to 20 by 2
    } yield {
      val newSet: AlgoSettings = algoSettings.copy(groupSize = pow)
      val taskCreation = (s: Int) => new RosenbrokeTask(s, newSet)
      new Analyzer(math.pow(2, 3).toInt, taskCreation)
    }
    val analyzers = analyzersPar.toList
    println(analyzers.head.task().name)
    print("groupSize; ")
    printParam(analyzers, _.task().algoSettings.groupSize)
    println()
    printAllInfo(analyzers)
  }

  def analyzeGroupAmount(): Unit = {
    val analyzersPar = for {
      pow <- 4 to 20 by 2
    } yield {
      val newSet: AlgoSettings = algoSettings.copy(groupAmount = pow)
      val taskCreation = (s: Int) => new RosenbrokeTask(s, newSet)
      new Analyzer(math.pow(2, 3).toInt, taskCreation)
    }
    val analyzers = analyzersPar.toList
    println(analyzers.head.task().name)
    print("groupAmount; ")
    printParam(analyzers, _.task().algoSettings.groupAmount)
    println()
    printAllInfo(analyzers)
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

  def printParam(analyzers: List[Analyzer], f: Analyzer => Double): Unit = {
    analyzers.map(f).map("%,.3f; ".format(_)).foreach(print)
  }

  def printWithMessage(analyzers: List[Analyzer], str: String, f: Analyzer => Double): Unit = {
    print(str + "; ")
    printParam(analyzers, f)
    println()
  }

  def printAllInfo(analyzers: List[Analyzer]): Unit = {
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
    ).foreach { case (name, func) => printWithMessage(analyzers, name, func) }
  }
}
