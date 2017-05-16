package generators

/**
  * Created by k.neyman on 29.03.2017.
  */
class DoubleGen extends Generator[Double] {
  protected val rand = new java.util.Random()

  override def generate: Double = rand.nextDouble()
}
