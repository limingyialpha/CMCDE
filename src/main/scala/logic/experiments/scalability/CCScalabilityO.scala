package logic.experiments.scalability

import io.github.edouardfouche.generators.{DataGenerator, Independent}
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import logic.utils.StopWatch
import breeze.stats.mean

/**
 * Scalability of different Measures for Canonical Correlation on observation number.
 *
 * This experiment analyse the runtime(CPU time) of different canonical correlation measures,
 * with respect to different observation numbers.
 * Only GMCDE is in scala, others are in Python partner repo.
 * We look at Independent Uniform distribution.
 * Both first and second groups have 2 dimensions, total 4.
 * We look at maximum 1000 observations.
 */
object CCScalabilityO extends Experiment {
  // data specific params
  val generator: DataGenerator = Independent(4, 0.0, "gaussian", 0)
  val dim_x = Set(0, 1)
  val dim_y = Set(2, 3)
  val observation_nums_of_interest = Vector(10, 20, 50, 100, 200, 300, 400, 500, 1000)

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
    info(s"generator: ${generator.id}")
    info(s"observation numbers of interest: ${observation_nums_of_interest mkString ","}")

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

    val attributes = List("measure", "obs_num", "avg_cpu_time")
    val summary = ExperimentSummary(attributes)
    for (obs_num <- observation_nums_of_interest) {
      info(s"now dealing with measure: GMCDE, observation number: $obs_num")
      val cpu_times = (1 to repetitions).par.map(_ => {
        val data = generator.generate(obs_num)
        StopWatch.measureCPUTime(measure.canonical_contrast(data, dim_x, dim_y))._1
      }).toVector
      val avg_cpu_time = mean(cpu_times)
      val to_write = List("GMCDE", obs_num, avg_cpu_time).mkString(",")
      summary.direct_write(summaryPath, to_write)
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
