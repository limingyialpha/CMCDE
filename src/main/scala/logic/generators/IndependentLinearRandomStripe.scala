package logic.generators

import breeze.stats.distributions.Uniform
import scala.util.Random
import io.github.edouardfouche.generators.ParameterizedDataGenerator

/**
 * This generator generates data in a stripe fashion.
 * some random dimensions are independent, some are linear
 */
case class IndependentLinearRandomStripe(nDim: Int, noise: Double, noisetype: String, discretize: Int)(linear_dim: Option[Double] = Some(nDim/2)) extends ParameterizedDataGenerator {
  override lazy val shortname = "ilrs"
  val ld: Int = linear_dim match {
    case Some(i) => i.asInstanceOf[Int]
    case None => nDim/2
  }
  val param: Double = ld
  val name = "independent_linear_random_stripe"

  protected def getPoints(n: Int): Array[Array[Double]] = {
    // the flag is true: this dimension is linear with noise
    // the flag is false: this dimension is independent with noise
    val flags = Random.shuffle((0 until nDim).toList).map(d => {
      if (d < ld) true else false
    })
    (1 to n).toArray.map { i => {
      val x = Uniform(0, 1).draw()
      (0 until nDim).toArray.map(d => {
        if (!flags(d)) {
          Uniform(0, 1).draw()
        } else x
      })
    }
    }
  }
}
