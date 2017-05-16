package generators

/**
  * Created by k.neyman on 29.03.2017.
  */
class ChooseIntGen(low: Int, high: Int) extends Generator[Int] {
  protected val rand = new java.util.Random()

  override def generate: Int = low + rand.nextInt(high - low)
}
