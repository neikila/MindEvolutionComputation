package generators

/**
  * Created by k.neyman on 29.03.2017.
  */
class IntGen extends Generator[Int] {
  protected val rand = new java.util.Random()

  override def generate: Int = rand.nextInt()
}
