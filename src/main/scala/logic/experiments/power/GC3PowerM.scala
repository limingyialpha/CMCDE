package logic.experiments.power

import breeze.stats.DescriptiveStats.percentile
import io.github.edouardfouche.generators._
import logic.data.Utility.round
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import breeze.stats.{stddev, mean}

/**
 * Statistical Power of different Measures for Generalized Contrast with 3 groups of dimensions
 *
 * Compare the power of GMCDE in general case with other competitors.
 * The general case we are looking at is:
 * Dependency between 3 groups of dimensions.
 * Other competitors are implemented in Python. See partner Repo.
 * We also look at different observation numbers, dimensions, noise levels,
 * symmetric data distributions of all kinds
 */
object GC3PowerM extends Experiment {
  // data specific params
  val generators: Vector[(Int, Double, String, Int) => DataGenerator] = Vector(
    Linear,
    DoubleLinear(_: Int, _: Double, _: String, _: Int)(Some(0.25)),
    LinearPeriodic(_: Int, _: Double, _: String, _: Int)(period = Some(2)),
    Sine(_: Int, _: Double, _: String, _: Int)(period = Some(1)),
    Sine(_: Int, _: Double, _: String, _: Int)(period = Some(5)),
    Hypercube,
    HypercubeGraph,
    HyperSphere,
    Cross,
    Star,
    Hourglass,
    Zinv,
  )
  val dimensions_of_interest = Vector(6, 9, 12, 15)
  val noise_levels = 30
  val noises_of_interest: Vector[Double] = (0 to noise_levels).toVector.map(x => round(x.toDouble / noise_levels.toDouble, 2))
  val observation_num_of_interest = Vector(100, 1000)

  // GMCDE specific params
  //val iteration_num = 50
  val iteration_num = 200
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for canonical correlation
  val slice_technique = "c" // redundant, since GMCDE uses it internally for canonical correlation
  val estimator = "ItGI" // redundant, since GMCDE uses it internally for canonical correlation
  val measure: GMCDE = GMCDE(parallelize, iteration_num)

  // methodology specific params
  val power_computation_iteration_num = 500

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    val gen_names = generators.map(g => g(2, 0.0, "gaussian", 0).name)
    info(s"generators of interest for both symmetric and asymmetric distributions : ${gen_names mkString ","}")
    info(s"dimensions of interest: ${dimensions_of_interest mkString ","}")
    info(s"noise levels: $noise_levels")
    info(s"observation numbers of interest: ${observation_num_of_interest mkString ","}")

    info(s"Dependency measure specific params:")
    info(s"Generalized Contrast measure: GMCDE")
    info(s"number of iterations: $iteration_num")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"number of iterations for power computation: $power_computation_iteration_num")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "dim", "noise", "obs_num", "measure", "avg_gc", "std_gc", "power90", "power95",
      "power99")
    val summary = ExperimentSummary(attributes)


    for (obs_num <- observation_num_of_interest) {
      for (dim <- dimensions_of_interest) {
        info(s"now computing thresholds for measure: GMCDE, observation number: $obs_num, dimension: $dim")
        val independent_benchmark_instance = Independent(dim, 0, "gaussian", 0)
        val independent_benchmark_generalized_contrasts = (1 to power_computation_iteration_num).par.map(_ => {
          val data = independent_benchmark_instance.generate(obs_num)
          val dim_x = (0 until dim / 3).toSet
          val dim_y = (dim / 3 until dim / 3 * 2).toSet
          val dim_z = (dim / 3 * 2 until dim).toSet
          val dims = Set(dim_x, dim_y, dim_z)
          measure.generalized_contrast(data, dims)
        }).toVector
        val threshold90 = percentile(independent_benchmark_generalized_contrasts, 0.90)
        val threshold95 = percentile(independent_benchmark_generalized_contrasts, 0.95)
        val threshold99 = percentile(independent_benchmark_generalized_contrasts, 0.99)
        info(s"finished computing thresholds for measure: GMCDE, observation number: $obs_num, dimension: $dim")

        for (noise <- noises_of_interest.par) {
          info(s"now dealing with gens: symmetric, measure: GMCDE, observation number: $obs_num, dimension: $dim, noise $noise")
          // symmetric case
          for (gen <- generators.par) {
            val generator_instance = gen(dim, noise, "gaussian", 0)
            val comparison_generalized_contrasts = (1 to power_computation_iteration_num).par.map(_ => {
              val data = generator_instance.generate(obs_num)
              val dim_x = (0 until dim / 3).toSet
              val dim_y = (dim / 3 until dim / 3 * 2).toSet
              val dim_z = (dim / 3 * 2 until dim).toSet
              val dims = Set(dim_x, dim_y, dim_z)
              measure.generalized_contrast(data, dims)
            }).toVector
            val power90 = comparison_generalized_contrasts.count(c => c > threshold90).toDouble / power_computation_iteration_num.toDouble
            val power95 = comparison_generalized_contrasts.count(c => c > threshold95).toDouble / power_computation_iteration_num.toDouble
            val power99 = comparison_generalized_contrasts.count(c => c > threshold99).toDouble / power_computation_iteration_num.toDouble
            val avg_gc = mean(comparison_generalized_contrasts)
            val std_gc = stddev(comparison_generalized_contrasts)
            val to_write = List(generator_instance.id, dim, noise, obs_num, "GMCDE", avg_gc, std_gc, power90, power95, power99).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
      }
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
