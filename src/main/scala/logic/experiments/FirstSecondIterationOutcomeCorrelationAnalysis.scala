package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, Hypercube, Independent, Linear}
import logic.generators.IndependentLinearStripe
import logic.gmcde.GMCDE

/**
 * This experiment is aimed at proving Section 5.1 Specifics of
 * attribute types in the paper
 * "A framework for dependency estimation in heterogeneous data streams"
 * wrong.
 * The paper implies that by looking at the distribution of approximated
 * contrast with 1 MC iterations, we can conclude the power of the estimator.
 * However, each iteration is correlated because they are dependent
 * on the data distribution and thus implies information of the data distribution.
 * In this experiment, we look at the distribution of the result from the
 * first 2 MC iterations and conclude:
 * 1. they are identically distributed
 * 2. they are strongly correlated and thus implies no evidence on "power"
 *
 * The parameters in the paper are:
 * 1. generator is 2 dimensional Independent or Linear with gaussian noise level 0.4
 * 2. 10000 iterations
 * 3. 1000 observations in each iteration
 * 4. slice technique "c"
 * 5. alpha 0.5
 * 6. estimator "R"
 */
object FirstSecondIterationOutcomeCorrelationAnalysis extends Experiment {
  // data specific params
  val dim = 2
  val generators: Vector[DataGenerator] = Vector(
    Independent(dim,0,"gaussian",0),
    Linear(dim,0.4,"gaussian",0),
    // we also look at the below
    Hypercube(dim,0.4,"gaussian",0),
    IndependentLinearStripe(dim,0.4,"gaussian",0)
  )
  val observation_num = 1000

  // GMCDE specific params
  val parallelize = 1
  val alpha = 0.5 // redundant, since GMCDE uses it internally for contrast
  val slice_technique = "c" // we believe center slice is the best
  val estimator: String = "R"

  // methodology params
  val repetitions = 10000

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")

    info("Data specific params:")
    val gen_names = generators.map(g => g.name)
    info(s"generators of interest for convergence analysis: ${gen_names mkString ","}")
    info(s"observation number: $observation_num")

    info(s"Dependency measure specific params:")
    info(s"Dependency measure: GMCDE")
    info(s"parallelization level in GMCDE: $parallelize")
    info(s"expected share of instances in slice, alpha: $alpha")
    info(s"slice technique: $slice_technique")
    info(s"dependency estimator: $estimator")

    info(s"Methodology specific params:")
    info(s"number of repetitions: $repetitions")

    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "rep", "first", "second")
    val summary = ExperimentSummary(attributes)
    for (gen_ins <- generators) {
          val gmcde = GMCDE(parallelize, 1)
          for (rep <- (1 to repetitions).par) {
            val data = gen_ins.generate(observation_num)
            val dims = (0 until dim).toSet
            val first_iteration_outcome = gmcde.contrast(data, dims)(estimator, slice_technique)
            val second_iteration_outcome = gmcde.contrast(data, dims)(estimator, slice_technique)
            val to_write = List(gen_ins.id, rep, first_iteration_outcome, second_iteration_outcome).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Finished experiments - ${this.getClass.getSimpleName}")
  }
}
