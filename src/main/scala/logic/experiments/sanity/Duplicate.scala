package logic.experiments.sanity

import io.github.edouardfouche.generators.{DataGenerator, Independent}
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import logic.utils.StopWatch
import breeze.stats.mean

/**
 * This experiment checks the behaviour of the measure with increasing dimensions of duplicates.
 * We start with 2 dimensions with identical observations, we then add duplicate-dimensions.
 * We expect the generalized contrast to stay the same or at least increase.
 * GMCDE is in scala repo and dHSIC is in python repo
 */
class Duplicate(output_folder: String) extends Experiment(output_folder) {
  // data specific params
  // actually not used, just for reference
  val duplicate: Array[Array[Double]] = (0 until 1000).map(x => Array(x.toDouble/1000.0)).toArray

  // GMCDE specific params
  val iteration_num = 50
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for canonical correlation
  val slice_technique = "c" // redundant, since GMCDE uses it internally for canonical correlation
  val estimator = "ItGI" // redundant, since GMCDE uses it internally for canonical correlation
  val measure: GMCDE = GMCDE(parallelize, iteration_num)

  // methodology params
  val maximal_duplicates = 100

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    info(s"Duplicate: 0 to 0.999 with step 0.001")

    info(s"Dependency measure specific params:")
    info(s"Generalized Contrast measure: GMCDE")
    info(s"number of iterations: $iteration_num")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"maximal duplicates: $maximal_duplicates")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("measure","duplicate", "gc")
    val summary = ExperimentSummary(attributes)

    for (i <- 2 to maximal_duplicates) {
      val data = (0 until 1000).map(x => (1 to i).map(d => x.toDouble/1000.0).toArray).toArray
      val dims = (0 until i).map(x => Set(x)).toSet
      val gc = measure.generalized_contrast(data, dims)
      val to_write = List("GMCDE", i, gc).mkString(",")
      summary.direct_write(summaryPath, to_write)
    }
  }
}
