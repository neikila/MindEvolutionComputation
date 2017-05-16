/**
  * Created by k.neyman on 15.05.2017.
  */
trait AnalyzerUtils {
  implicit class SummableIterable[T](val l: Iterable[T])(implicit numeric: Numeric[T]) {
    def average: Double = numeric.toDouble(l.sum) / l.size
    def squareDeviation: Double = math sqrt poweredSquareDeviation
    def deltas: Iterable[Double] = (l.drop(1) zip l).map { case (cur, prev) => cur - prev }
    def standardDeviation = math.sqrt(poweredSquareDeviation * (l.size - 1) / (l.size - 2) )

    private def poweredSquareDeviation: Double = {
      val av = average
      l.map { diff => math.pow(diff - av, 2) }.sum / l.size
    }
  }

  implicit def toDouble[T](t: T)(implicit numeric: Numeric[T]): Double = numeric.toDouble(t)
}
