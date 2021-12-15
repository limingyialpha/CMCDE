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
class ConvDEAndACDistr(output_folder: String) extends Experiment(output_folder) {
  // data specific params
  val generators: Vector[(Int, Double, String, Int) => DataGenerator] = Vector(
    Independent,
    Linear,
    Hypercube,
    IndependentLinearStripe
  )
  val dimension = 10
  val noise = 0.2
  val observation_num = 1000

  // GMCDE specific params
  val maximum_interested_iteration_number = 100
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for contrast
  val slice_technique = "c" // we believe center slice is the best
  val estimators_of_interest: Array[String] = Array("R", "ItGR", "ItGI", "ItGIBEV")

  // methodology params
  val repetitions_for_variance_estimation = 500

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    val gen_names = generators.map(g => g(2, 0.0, "gaussian", 0).name)
    info(s"generators of interest for convergence analysis: ${gen_names mkString ","}")
    info(s"dimension: $dimension")
    info(s"noise $noise")
    info(s"observation number: $observation_num")

    info(s"Dependency measure specific params:")
    info(s"Dependency measure: GMCDE")
    info(s"maximum interested iteration number: $maximum_interested_iteration_number")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"dependency estimators of interest: ${estimators_of_interest mkString ","}")

    info(s"Methodology specific params:")
    info(s"number of repetitions for variance estimation: $repetitions_for_variance_estimation")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "estimator", "iteration_num", "rep", "contrast", "cpu_time")
    val summary = ExperimentSummary(attributes)
    for (gen <- generators) {
      val gen_ins = gen(dimension, noise, "gaussian", 0)
      for (estimator <- estimators_of_interest) {
        info(s"now dealing with generator: ${gen_ins.id}, estimator $estimator")
        for (iteration_num <- (1 to maximum_interested_iteration_number).par) {
          val measure = GMCDE(parallelize, iteration_num)
          for (rep <- (1 to repetitions_for_variance_estimation).par) {
            val data = gen_ins.generate(observation_num)
            val dims = (0 until dimension).toSet
            val (cpu_time, contrast) = StopWatch.measureCPUTime(measure.contrast(data, dims)(estimator, slice_technique))
            val to_write = List(gen_ins.id, estimator, iteration_num, rep, contrast, cpu_time).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
      }
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
