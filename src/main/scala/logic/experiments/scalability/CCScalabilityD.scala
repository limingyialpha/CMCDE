package logic.experiments.scalability

import io.github.edouardfouche.generators.{DataGenerator, Independent}
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import logic.utils.StopWatch
import breeze.stats.mean

/**
 * Scalability of different Measures for Canonical Correlation on dimension.
 *
 * This experiment analyse the runtime(CPU time) of different canonical correlation measures,
 * with respect to dimensions.
 * Only GMCDE is in scala, others are in Python partner repo.
 * We look at Independent Uniform distribution.
 * Each group has equal number of dimensions
 * Observation number is 1000.
 * We look at maximum 10 dimensions.
 */
object CCScalabilityD extends Experiment {
  // data specific params
  val generator: (Int, Double, String, Int) => DataGenerator = Independent
  val noise = 0
  val dimensions_of_interest = Vector(2, 4, 6, 8, 10)
  val observation_num = 1000

  // GMCDE specific params
  val iteration_num = 50
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for canonical correlation
  val slice_technique = "c" // redundant, since GMCDE uses it internally for canonical correlation
  val estimator = "ItGI" // redundant, since GMCDE uses it internally for canonical correlation
  val measure: GMCDE = GMCDE(parallelize, iteration_num)

  // methodology params
  val repetitions = 500
  val unit = "ms"

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    info(s"generator: ${generator(2,0.0,"gaussian",0).name}")
    info(s"dimensions of interest: ${dimensions_of_interest mkString ","}")
    info(s"observation number: $observation_num")

    info(s"Dependency measure specific params:")
    info(s"Canonical Correlation measure: GMCDE")
    info(s"number of iterations: $iteration_num")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"number of repetitions: $repetitions")
    info(s"unit of runtime (cpu time): $unit")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("measure", "dim", "avg_cpu_time")
    val summary = ExperimentSummary(attributes)
    for (dim <- dimensions_of_interest) {
      info(s"now dealing with measure: GMCDE, dimension: $dim")
      val gen_ins = generator(dim, noise,"gaussian", 0)
      val dim_x = (0 until dim/2).toSet
      val dim_y = (dim/2 until dim).toSet
      val cpu_times = (1 to repetitions).par.map(_ => {
        val data = gen_ins.generate(observation_num)
        StopWatch.measureCPUTime(measure.canonical_contrast(data, dim_x, dim_y))._1
      }).toVector
      val avg_cpu_time = mean(cpu_times)
      val to_write = List("GMCDE", dim, avg_cpu_time).mkString(",")
      summary.direct_write(summaryPath, to_write)
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
