package generators

/**
  * Created by k.neyman on 12.05.2017.
  */
class NormalGen(val sigma: Double = 1, val base: Double = 0) extends Generator[Double] {
  protected val randR = new ChooseDoubleGen(0.001, 1)(new DoubleGen)
  protected val randTeta = new ChooseDoubleGen(0, 1)(new DoubleGen).map(_ * 2 * math.Pi)

  override def generate: Double =
    base + sigma * (
      math.sqrt(-2 * math.log(randR.generate)) *
      math.cos(randTeta.generate))
}
