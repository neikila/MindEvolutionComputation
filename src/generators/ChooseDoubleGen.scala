package generators

/**
  * Created by k.neyman on 29.03.2017.
  */
class ChooseDoubleGen(low: Double, high: Double)(implicit val rand: Generator[Double]) extends Generator[Double] {
  override def generate: Double = low + (high - low) * rand.generate
}

object ChooseDoubleGen {
  def from(average: Double, amplitude: Double)(implicit rand: Generator[Double]) =
    new ChooseDoubleGen(average - amplitude, average + amplitude)(rand)
}
