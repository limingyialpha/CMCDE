package logic.experiments.scalability

import io.github.edouardfouche.generators.{DataGenerator, Independent}
import logic.experiments.Experiment
import logic.gmcde.GMCDE
import logic.utils.StopWatch
import breeze.stats.mean

/**
 * Scalability of different Measures for Generalized Contrast with 3 groups of dimensions on dimension.
 *
 * This experiment analyse the runtime(CPU time) of different general dependency measures,
 * with respect to dimensions.
 * Only GMCDE is in scala, others are in Python partner repo.
 * We look at Independent Uniform distribution.
 * We look at 3 groups of dimensions.
 * Each group has equal number of dimensions
 * Observation number is 1000.
 */
case class GC3ScalabilityD(output_folder: String) extends Experiment(output_folder) {
  // data specific params
  val generator: (Int, Double, String, Int) => DataGenerator = Independent
  val noise = 0
  val dimensions_of_interest = Vector(3, 6, 9, 12, 15)
  val observation_num = 1000

  // GMCDE specific params
  val iteration_num = 50
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for canonical correlation
  val slice_technique = "c" // redundant, since GMCDE uses it internally for canonical correlation
  val estimator = "ItGI" // redundant, since GMCDE uses it internally for canonical correlation
  val measure: GMCDE = GMCDE(parallelize, iteration_num)

  // methodology params
  val repetitions = 10000
  val unit = "ms"
  val pre_run_repetitions = 10000

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    info(s"generator: ${generator(2, 0.0, "gaussian", 0).name}")
    info(s"dimensions of interest: ${dimensions_of_interest mkString ","}")
    info(s"observation number: $observation_num")

    info(s"Dependency measure specific params:")
    info(s"Measure: GMCDE")
    info(s"number of iterations: $iteration_num")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"number of repetitions: $repetitions")
    info(s"unit of runtime (cpu time): $unit")
    info(s"pre-run repetitions: $pre_run_repetitions")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("measure", "dim", "avg_cpu_time")
    val summary = ExperimentSummary(attributes)

    // a pre-run to deal with missing optimizations for initial iterations
    // later iterations will be automatically optimized by the system, thus faster
    // initial one do not, as a result, we try to let the program run for a while then start
    for (dim <- dimensions_of_interest) {
      info(s"now doing pre-run with measure: GMCDE, dimension: $dim")
      val gen_ins = generator(dim, noise, "gaussian", 0)
      val dim_x = (0 until dim / 3).toSet
      val dim_y = (dim / 3 until dim / 3 * 2).toSet
      val dim_z = (dim / 3 * 2 until dim).toSet
      val dim_groups = Set(dim_x, dim_y, dim_z)
      val cpu_times = (1 to repetitions).par.map(_ => {
        val data = gen_ins.generate(observation_num)
        StopWatch.measureCPUTime(measure.generalized_contrast(data, dim_groups))._1
      }).toVector
    }


    for (dim <- dimensions_of_interest) {
      info(s"now dealing with measure: GMCDE, dimension: $dim")
      val gen_ins = generator(dim, noise, "gaussian", 0)
      val dim_x = (0 until dim / 3).toSet
      val dim_y = (dim / 3 until dim / 3 * 2).toSet
      val dim_z = (dim / 3 * 2 until dim).toSet
      val dim_groups = Set(dim_x, dim_y, dim_z)
      val cpu_times = (1 to repetitions).par.map(_ => {
        val data = gen_ins.generate(observation_num)
        StopWatch.measureCPUTime(measure.generalized_contrast(data, dim_groups))._1
      }).toVector
      val avg_cpu_time = mean(cpu_times)
      val to_write = List("GMCDE", dim, avg_cpu_time).mkString(",")
      summary.direct_write(summaryPath, to_write)
    }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
