package algo

/**
  * Created by k.neyman on 29.03.2017.
  */
object SearchAlgo {
  class Function(val f: List[Double] => Double) {
    var amount = 0

    def incr() = this.synchronized { amount += 1 }

    def apply(l: List[Double]): Double = {
      incr()
      f(l)
    }
  }
}
