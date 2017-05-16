package algo

/**
  * Created by k.neyman on 29.03.2017.
  */
case class Individual(xs: List[Double]) {
  def count(implicit f: SearchAlgo.Function) = f(xs)
}
