package generators

/**
  * Created by k.neyman on 29.03.2017.
  */
class ListBasedGen(private val base: List[Double],
                   private val distributions: List[Double])
                  (implicit val rand: Generator[Double])
  extends Generator[List[Double]] {

  private val doubleGens: List[Generator[Double]] =
    (base zip distributions).map { case (average, distr) =>
      new NormalGen(distr, average)
    }

  override def generate: List[Double] = doubleGens.map(gen => gen.generate)
}
