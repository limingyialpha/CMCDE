package logic.generators

import breeze.stats.distributions.Uniform
import io.github.edouardfouche.generators.DataGenerator

/**
 * This generator generates data in a stripe fashion.
 * for example: 7 dimension
 * The observation looks like (0.12, 0.44, 0.43, 0.44, 0.96, 0.44, 0.73)
 * dimension 1,3,5,7 are independent
 * dimension 2,4,6 are linear
 */
case class IndependentLinearStripe(nDim: Int, noise: Double, noisetype: String, discretize: Int) extends DataGenerator {
  val name = "independent_linear_stripe"
  override lazy val shortname = "ils"

  protected def getPoints(n: Int): Array[Array[Double]] = {
    (1 to n).toArray.map { i => {
      val x = Uniform(0, 1).draw()
      (1 to nDim).toArray.map(d => {
        if (d % 2 == 1) {
          Uniform(0, 1).draw()
        } else x

      })
    }
    }
  }
}
