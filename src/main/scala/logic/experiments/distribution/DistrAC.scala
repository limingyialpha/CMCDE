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
  val independent_generator: DataGenerator = Independent(dimension, 0.0, "gaussian", 0)
  val linear_generator: DataGenerator = Linear(dimension, 0.5, "gaussian",0)
  val observation_num = 1000

  // GMCDE specific params
  val iteration_numbers_of_interest: Array[Int] = Array(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 500, 1000)
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for contrast
  val slice_technique = "c" // we believe center slice is the best
  val estimator: String = "R"

  // methodology params
  val repetitions_for_histogram = 10000


  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    info(s"we are comparing approximated contrast distribution from ${independent_generator.id} and ${linear_generator.id}")
    info(s"dimension: $dimension")
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

    // generating samples for plotting
    val sample_i = independent_generator.generate(observation_num)
    val img_path_i = experiment_folder + "/" + "i" + "_scala" + ".csv"
    utils.saveDataSet(sample_i, img_path_i)
    val sample_l = Linear(2,0.0,"gaussian",0).generate(observation_num)
    val img_path_l = experiment_folder + "/" + "l" + "_scala" + ".csv"
    utils.saveDataSet(sample_l, img_path_l)
    val sample_l_with_noise = linear_generator.generate(observation_num)
    val img_path_l_with_noise = experiment_folder + "/" + "l_noise" + "_scala" + ".csv"
    utils.saveDataSet(sample_l_with_noise, img_path_l_with_noise)


    // independent case
    info("now starting experiments on independent case:")
    for (num_it <- iteration_numbers_of_interest) {
      info(s"now dealing with iteration number: $num_it.")
      val measure = GMCDE(parallelize, num_it)
      for (rep <- (1 to repetitions_for_histogram).par) {
        val data = independent_generator.generate(observation_num)
        val dims = (0 until dimension).toSet
        val contrast = measure.contrast(data, dims)(estimator, slice_technique)
        val to_write = List(independent_generator.id, num_it, independent_generator.noise, rep, contrast).mkString(",")
        summary.direct_write(summaryPath, to_write)
      }
    }
    // linear 0.5 case
    info("now starting experiments on linear 0.5 case:")
    for (num_it <- iteration_numbers_of_interest) {
      info(s"now dealing with iteration number: $num_it.")
      val measure = GMCDE(parallelize, num_it)
      for (rep <- (1 to repetitions_for_histogram).par) {
        val data = linear_generator.generate(observation_num)
        val dims = (0 until dimension).toSet
        val contrast = measure.contrast(data, dims)(estimator, slice_technique)
        val to_write = List(linear_generator.id, num_it, linear_generator.noise, rep, contrast).mkString(",")
        summary.direct_write(summaryPath, to_write)
      }
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
