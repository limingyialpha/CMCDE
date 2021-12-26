package logic.generators

import breeze.stats.distributions.Uniform
import scala.util.Random
import io.github.edouardfouche.generators.ParameterizedDataGenerator

/**
 * This generator generates groups of equal size linear dimensions.
 * Without noise, the dimensions in each group are perfectly linear correlated,
 * and the dimensions from different groups are perfectly independent.
 * We use this distribution to test if a dependency measure for n multivariate
 * random variables is able to ignore the dependencies within the dimensions
 *
 * of the random variables themselves
 */
case class GroupsOfLinears(nDim: Int, noise: Double, noisetype: String, discretize: Int)(num_groups: Option[Double]) extends ParameterizedDataGenerator {
  override lazy val shortname = "gl"
  val ngs: Int = num_groups match {
    case Some(i) => i.asInstanceOf[Int]
    case None => 1 // in this case, the generate degenerates to Linear generator
  }
  require(nDim % ngs == 0, "The number of dimension should be multiple of number of groups.")
  val param: Double = ngs
  val name = "groups_of_linears"
  val dims_in_each_group: Int = nDim / ngs

  protected def getPoints(n: Int): Array[Array[Double]] = {
    (1 to n).toArray.map { i => {
      val values_of_each_group = (0 until ngs).map(_ => Uniform(0, 1).draw())
      (0 until nDim).toArray.map(d => {
        val group = d / dims_in_each_group
        values_of_each_group(group)
      })
    }
    }
  }
}
