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
    println(List(1.0, 3.0, 5.0).average)

    //    println(new RosenbrokeTask(size))
    //    new Analyzer(() => { new RastriginTask(math.pow(2, 2).toInt) })
//    println(new RosenbrokeTask(32, true).optimum)

//    createAnalyze()
  }

  def createAnalyze() {
    val analyzersPar = for {
      i <- (1 to 5).toParArray
      t <- tasks.take(1)
    } yield new Analyzer(math.pow(2, i).toInt, t)

    val analyzers = analyzersPar.toList
    println("Error")
    analyzers.map(_.error).map("%,f ".format(_)).foreach(println)

    println("sqDeviation")
    analyzers.map(_.sqDeviation).map("%,f ".format(_)).foreach(println)

    println("average")
    analyzers.map(_.averageBest).map("%,f ".format(_)).foreach(println)
  }
}
