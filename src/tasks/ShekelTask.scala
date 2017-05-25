package tasks

import algo.SearchAlgo
import generators.{ChooseDoubleGen, DoubleGen}

/**
  * Created by k.neyman on 27.04.2017.
  */
class ShekelTask(size: Int) extends Task("Shekel", size) {
  private val f0s: List[Double] = 0.3 :: 0.9 :: 0.7 :: 0.8 :: 0.1 :: 0.01 :: 0.65 :: Nil
  private val X: List[List[Double]] = List.tabulate(f0s.size) { _ =>
    implicit val gen = new DoubleGen
    bounds.map { case (bottom, up) => new ChooseDoubleGen(bottom, up).generate }
  }



  println((f0s zip X).mkString("\n"))

  override val realOptimum: Double = 1 / f0s.min

  private def znam(xs: List[Double], f0: Double, Xs: List[Double]): Double = f0 + (xs zip Xs).map { case (a, b) => math.pow(a - b, 2) }.sum

  val f = new SearchAlgo.Function(list => (f0s zip X).map { case (f0, x) => 1.0 / znam(list, f0, x) }.sum)

  // wrong
  override val realOptimumXs: List[Double] = (for { _ <- 0 until size } yield 0.0).toList
}
