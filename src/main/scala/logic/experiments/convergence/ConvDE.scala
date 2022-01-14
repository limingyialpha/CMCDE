package logic.experiments.convergence

import io.github.edouardfouche.generators.{DataGenerator, Hypercube, Independent, Linear}
import logic.experiments.Experiment
import logic.generators.IndependentLinearStripe
import logic.gmcde.GMCDE
import logic.utils.StopWatch

/**
 * Convergence of different Dependency Estimators and Approximated Contrast Distribution
 *
 * We look at the variance and runtime of approximated contrast of different estimators,
 * with respect to iteration numbers.
 * We also look at the approximated contrast distribution with respect to iteration numbers
 */
case class ConvDE(output_folder: String) extends Experiment(output_folder) {
  // data specific params
  val dimension = 10
  val generators: Vector[DataGenerator] = Vector(
    //Independent(dimension,0.0,"gaussian",0),
    Linear(dimension, 0.4, "gaussian", 0),
//  Hypercube(dimension, 0.2, "gaussian", 0),
    //IndependentLinearStripe(dimension, 0.2, "gaussian", 0)
  )
  val observation_num = 1000

  // GMCDE specific params
  val maximum_interested_iteration_number = 100
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for contrast
  val slice_technique = "c" // we believe center slice is the best
  val estimators_of_interest: Array[String] = Array("R", "ItR", "ItGR", "ItGI", "ItGIBEV")

  // methodology params
  val repetitions_for_variance_estimation = 40000

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    val gen_ids = generators.map(g => g.id)
    info(s"generators of interest for convergence analysis: ${gen_ids mkString ","}")
    info(s"dimension: $dimension")
    info(s"observation number: $observation_num")

    info(s"Dependency measure specific params:")
    info(s"Measure: GMCDE")
    info(s"maximum interested iteration number: $maximum_interested_iteration_number")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"dependency estimators of interest: ${estimators_of_interest mkString ","}")

    info(s"Methodology specific params:")
    info(s"number of repetitions for variance estimation: $repetitions_for_variance_estimation")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "estimator", "iteration_num", "rep", "contrast", "cpu_time", "real_contrast")
    val summary = ExperimentSummary(attributes)

    for (gen <- generators) {
      val data = gen.generate(observation_num)
      val dims = (0 until dimension).toSet
      val real_contrast = GMCDE(parallelize, 1000).contrast(data, dims)("R", slice_technique)
      for (estimator <- estimators_of_interest) {
        info(s"now dealing with generator: ${gen.id}, estimator $estimator")
        for (iteration_num <- (1 to maximum_interested_iteration_number).par) {
          val measure = GMCDE(parallelize, iteration_num)
          for (rep <- (1 to repetitions_for_variance_estimation).par) {
            val (cpu_time, contrast) = StopWatch.measureCPUTime(measure.contrast(data, dims)(estimator, slice_technique))
            val to_write = List(gen.id, estimator, iteration_num, rep, contrast, cpu_time, real_contrast).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
      }
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
