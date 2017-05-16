package generators

/**
  * Created by k.neyman on 29.03.2017.
  */
class OneOfGen[T](list: List[T]) extends Generator[T] {
  private val chooseGen = new ChooseIntGen(0, list.size)

  override def generate: T = list(chooseGen.generate)
}
