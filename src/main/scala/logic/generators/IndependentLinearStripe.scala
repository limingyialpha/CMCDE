package logic.generators

import breeze.stats.distributions.Uniform
import io.github.edouardfouche.generators.DataGenerator

case class IndependentLinearStripe(nDim: Int, noise: Double, noisetype: String, discretize: Int) extends DataGenerator {
  val name = "independent_linear_stripe"

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
