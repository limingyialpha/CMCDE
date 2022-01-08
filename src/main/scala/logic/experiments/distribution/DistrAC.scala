package logic.experiments.distribution

import io.github.edouardfouche.generators.{DataGenerator, Independent, Linear}
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import logic.data.Utility.round

/**
 * Distributions of approximated contrast
 */
case class DistrAC(output_folder: String) extends Experiment(output_folder) {
  // data specific params
  val dimension = 2
  val benchmark_generator: DataGenerator = Independent(dimension, 0.0, "gaussian", 0)
  val generator: (Int, Double, String, Int) => DataGenerator = Linear
  val noise_levels = 30
  val noises_of_interest: Vector[Double] = (0 to noise_levels).toVector.map(x => round(x.toDouble / noise_levels.toDouble, 2))
  val observation_num = 1000

  // GMCDE specific params
  val iteration_numbers_of_interest: Array[Int] = Array(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 500, 1000)
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for contrast
  val slice_technique = "c" // we believe center slice is the best
  val estimator: String = "R"

  // methodology params
  val repetitions_for_histogram = 5000

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    info("we are comparing independent approximated contrast distribution with linear ones")
    info(s"dimension: $dimension")
    info(s"noise levels: $noise_levels")
    info(s"observation number: $observation_num")

    info(s"Dependency measure specific params:")
    info(s"Measure: GMCDE")
    info(s"iteration numbers of interest: ${iteration_numbers_of_interest mkString ","}")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"dependency estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"repititions for histogram: $repetitions_for_histogram")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "iteration_num", "noise", "rep", "contrast")
    val summary = ExperimentSummary(attributes)
    // independent case
    info("now starting experiments on independent case:")
    for (num_it <- iteration_numbers_of_interest) {
      info(s"now dealing with iteration number: $num_it.")
      val measure = GMCDE(parallelize, num_it)
      for (rep <- (1 to repetitions_for_histogram).par) {
        val data = benchmark_generator.generate(observation_num)
        val dims = (0 until dimension).toSet
        val contrast = measure.contrast(data, dims)(estimator, slice_technique)
        val to_write = List(benchmark_generator.id, num_it, 0.0, rep, contrast).mkString(",")
        summary.direct_write(summaryPath, to_write)
      }
    }
    // for comparison
    info("now starting experiments on linear cases:")
    for (num_it <- iteration_numbers_of_interest) {
      info(s"now dealing with iteration number: $num_it.")
      val measure = GMCDE(parallelize, num_it)
      for (noise <- noises_of_interest.par) {
        val gen = generator(dimension, noise, "gaussian", 0)
        for (rep <- (1 to repetitions_for_histogram).par) {
          val data = gen.generate(observation_num)
          val dims = (0 until dimension).toSet
          val contrast = measure.contrast(data, dims)(estimator, slice_technique)
          val to_write = List(gen.id, num_it, noise, rep, contrast).mkString(",")
          summary.direct_write(summaryPath, to_write)
        }
      }
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
