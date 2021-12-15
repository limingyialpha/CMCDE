package logic.experiments.sanity

import io.github.edouardfouche.generators.{DataGenerator, Independent}
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import logic.utils.StopWatch
import breeze.stats.mean

/**
 * This experiment checks the behaviour of the measure with increasing independent dimensions.
 * We start with 2 dimensions with identical observations, we then add independent dimensions.
 * We expect the generalized contrast to drop.
 * GMCDE is in scala repo and dHSIC is in python repo
 */
class Dilute(output_folder: String) extends Experiment(output_folder) {
  // data specific params
  val data_12: Array[Array[Double]] = (0 until 1000).map(x => Array(x.toDouble/1000.0, x.toDouble/1000.0)).toArray

  // GMCDE specific params
  val iteration_num = 50
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for canonical correlation
  val slice_technique = "c" // redundant, since GMCDE uses it internally for canonical correlation
  val estimator = "ItGI" // redundant, since GMCDE uses it internally for canonical correlation
  val measure: GMCDE = GMCDE(parallelize, iteration_num)

  // methodology params
  val maximal_extra_independent_dimensions_num = 100

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    info(s"benchmark: 2 dimensions of 0 to 0.999 with step 0.001")

    info(s"Dependency measure specific params:")
    info(s"Generalized Contrast measure: GMCDE")
    info(s"number of iterations: $iteration_num")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"maximal extra independent dimensions: $maximal_extra_independent_dimensions_num")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("measure","dilution", "gc")
    val summary = ExperimentSummary(attributes)

    for (i <- 0 to maximal_extra_independent_dimensions_num) {
      val data = if (i == 0) data_12 else {
        val independence = Independent(i, 0, "gaussian",0).generate(data_12.length)
        data_12.zip(independence).map(tuple => tuple._1 ++ tuple._2)
      }
      val dims = (0 until 2 + i).map(x => Set(x)).toSet
      val gc = measure.generalized_contrast(data, dims)
      val to_write = List("GMCDE", i, gc).mkString(",")
      summary.direct_write(summaryPath, to_write)
    }
  }
}
