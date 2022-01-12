package logic.experiments.distribution

import io.github.edouardfouche.generators.DataGenerator
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import logic.generators.IndependentLinearStripe
import logic.index.RankIndex
import logic.stats.mcde.KSP

/**
 * Distributions of iteration values w.r.t different reference dimensions.
 * We focus on 4-dimensional IndependentLinearStripe observation-distribution
 */
case class DistrIV(output_folder: String) extends Experiment(output_folder) {
  // data specific params
  val dimension = 4
  val generator: DataGenerator = IndependentLinearStripe(dimension, 0.5 , "gaussian", 0)
  val observation_num = 1000

  // GMCDE specific params
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for contrast
  val slice_technique = "c" // we believe center slice is the best
  val estimator: String = "R"

  // methodology params
  val repetitions_for_histogram = 10000


  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    info(s"we look at ${generator.id}")
    info(s"dimension: $dimension")
    info(s"observation number: $observation_num")

    info(s"Dependency measure specific params:")
    info(s"Measure: GMCDE")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"dependency estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"repititions for histogram: $repetitions_for_histogram")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "noise", "ref_dim", "rep", "value", "contrast")
    val summary = ExperimentSummary(attributes)

    val sample = generator.generate(observation_num)
    val img_path = experiment_folder + "/" + "image" + "_scala" + ".csv"
    utils.saveDataSet(sample, img_path)
    val m = new RankIndex(sample)
    val dims_set = (0 until dimension).toSet
    val ksp = KSP()
    val contrast = GMCDE(parallelize, 10000).contrast(sample, dims_set)(estimator, slice_technique)
    info(s"The contrast is $contrast")
    for (r <- 0 until dimension) {
      info(s"Now dealing with reference dimension: $r")
      for (rep <- (1 to repetitions_for_histogram).par) {
        val value = ksp.get_value_by_ref_dim(m, dims_set, r)(slice_technique)
        val to_write = List(generator.id, generator.noise, r, rep, value, contrast).mkString(",")
        summary.direct_write(summaryPath, to_write)
      }
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
