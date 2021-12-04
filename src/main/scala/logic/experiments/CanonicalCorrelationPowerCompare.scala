package logic.experiments

import breeze.stats.DescriptiveStats.percentile
import breeze.stats.{mean, stddev}
import io.github.edouardfouche.generators._
import logic.data.Utility.round
import logic.gmcde.GMCDE

/**
 * Compare the power of GMCDE in canonical correlation case with other competitors.
 * Other competitors are implemented in Python. See partner Repo.
 * We also look at different observation numbers, dimensions, noise levels,
 * symmetric/asymmetric data distributions of all kinds
 */
object CanonicalCorrelationPowerCompare extends Experiment {
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
  val dimensions_of_interest = Vector(4, 8, 12, 16)
  val noise_levels = 30
  val noises_of_interest: Array[Double] = (0 to noise_levels).toArray.map(x => round(x.toDouble / noise_levels.toDouble, 2))
  val observation_num_of_interest = Vector(100, 1000)

  // GMCDE specific params
  val iteration_num = 50
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for canonical correlation
  val slice_technique = "c" // redundant, since GMCDE uses it internally for canonical correlation
  val estimator = "ItGI" // redundant, since GMCDE uses it internally for canonical correlation
  val gmcde: GMCDE = GMCDE(parallelize, iteration_num)

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
    info(s"Canonical Correlation measure: GMCDE")
    info(s"number of iterations: $iteration_num")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"number of iterations for power computation: $power_computation_iteration_num")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "dim", "noise", "obs_num", "measure", "avg_cc", "std_cc", "power90", "power95",
      "power99")
    val summary = ExperimentSummary(attributes)


    for (obs_num <- observation_num_of_interest) {
      for (dim <- dimensions_of_interest) {
        info(s"now computing thresholds for measure: GMCDE, observation number: $obs_num, dimension: $dim")
        val independent_benchmark_instance = Independent(dim, 0, "gaussian", 0)
        val independent_benchmark_canonical_contrasts = (1 to power_computation_iteration_num).par.map(_ => {
          val data = independent_benchmark_instance.generate(obs_num)
          val dim_x = (0 until dim / 2).toSet
          val dim_y = (dim / 2 until dim).toSet
          gmcde.canonical_contrast(data, dim_x, dim_y)
        }).toVector
        val threshold90 = percentile(independent_benchmark_canonical_contrasts, 0.90)
        val threshold95 = percentile(independent_benchmark_canonical_contrasts, 0.95)
        val threshold99 = percentile(independent_benchmark_canonical_contrasts, 0.99)
        info(s"finished computing thresholds for measure: GMCDE, observation number: $obs_num, dimension: $dim")

        for (noise <- noises_of_interest.par) {
          info(s"now dealing with gens: symmetric, measure: GMCDE, observation number: $obs_num, dimension: $dim, noise $noise")
          // symmetric case
          for (gen <- generators.par) {
            val generator_instance = gen(dim, noise, "gaussian", 0)
            val comparison_canonical_contrasts = (1 to power_computation_iteration_num).par.map(_ => {
              val data = generator_instance.generate(obs_num)
              val dim_x = (0 until dim / 2).toSet
              val dim_y = (dim / 2 until dim).toSet
              gmcde.canonical_contrast(data, dim_x, dim_y)
            }).toVector
            val power90 = comparison_canonical_contrasts.count(c => c > threshold90).toDouble / power_computation_iteration_num.toDouble
            val power95 = comparison_canonical_contrasts.count(c => c > threshold95).toDouble / power_computation_iteration_num.toDouble
            val power99 = comparison_canonical_contrasts.count(c => c > threshold99).toDouble / power_computation_iteration_num.toDouble
            val avg_cc = mean(comparison_canonical_contrasts)
            val std_cc = stddev(comparison_canonical_contrasts)
            val to_write = List(generator_instance.id, dim, noise, obs_num, "GMCDE", avg_cc, std_cc, power90, power95, power99).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
          // asymmetric case
          info(s"now dealing with gens: asymmetric, measure: GMCDE, observation number: $obs_num , dimension: $dim, noise $noise")
          for (gen <- generators.par) {
            val generator_instance = gen(dim / 2, noise, "gaussian", 0)
            val comparison_canonical_contrasts = (1 to power_computation_iteration_num).par.map(_ => {
              val data_sy = generator_instance.generate(obs_num)
              val data_asy = Independent(dim / 2, 0, "gaussian", 0).generate(obs_num)
              val data = data_sy.zip(data_asy).map(tuple => tuple._1 ++ tuple._2)
              val dim_x = (0 until dim / 4).toSet union (dim / 2 until dim / 4 * 3).toSet
              val dim_y = (0 until dim).toSet diff dim_x
              gmcde.canonical_contrast(data, dim_x, dim_y)
            }).toVector
            val power90 = comparison_canonical_contrasts.count(c => c > threshold90).toDouble / power_computation_iteration_num.toDouble
            val power95 = comparison_canonical_contrasts.count(c => c > threshold95).toDouble / power_computation_iteration_num.toDouble
            val power99 = comparison_canonical_contrasts.count(c => c > threshold99).toDouble / power_computation_iteration_num.toDouble
            val avg_cc = mean(comparison_canonical_contrasts)
            val std_cc = stddev(comparison_canonical_contrasts)
            val to_write = List(generator_instance.id + "_asy", dim, noise, obs_num, "GMCDE", avg_cc, std_cc, power90, power95, power99).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
      }
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}