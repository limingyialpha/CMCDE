package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, Independent, Linear}
import logic.experiments.ContrastApproximationConvergenceSpeed.{alpha, experiment_folder, generators, info, ksp, nrep, summaryPath}
import logic.index.RankIndex
import logic.stats.mcde.KSP

/**
 * This experiment is aimed at proving Section 5.1 Specifics of
 * attribute types in the paper
 * "A framework for dependency estimation in heterogeneous data streams"
 * wrong. The paper implies that by looking at the distribution of approximated
 * contrast with 1 MC iterations, we can conclude the power of the estimator.
 * However, each iteration is strongly correlated because they are dependent
 * on the data distribution and thus implies information of the data distribution.
 * In this experiment, we look at the distribution of the result from the
 * first 2 MC iterations and conclude:
 * 1. they are identically distributed
 * 2. they are strongly correlated and thus implies no evidence on "power"
 *
 * The parameters in the paper are:
 * 1. generator is Independent or Linear with gaussian noise level 0.4
 * 2. 10000 iterations
 * 3. 1000 observations in each iteration
 * 4. MC iteration technique is choosing random reference dimension
 * 5. alpha 0.5
 */
object ProvePaperWrong extends Experiment {
  val alpha = 0.5
  val generators: Vector[DataGenerator] = Vector(
    Independent(2,0,"gaussian", 0),
    Linear(2,0.4,"gaussian",0)
  )
  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)

  // for debugging purpose
  //val nrep = 10
  val nrep = 100000
  val num_obs = 1000
  val MC_num = 2

  def run() = {
    info(s"Starting experiments")
    val attributes = List("genId", "rep") ++ (1 to MC_num).map(x => x.toString).toList
    val summary = ExperimentSummary(attributes)
    for {
      generator <- generators.par
    } {
      for{rep <- 1 to nrep}{
        if(rep % (nrep/10) == 0) {
          info(s"Generator id: ${generator.id}, Reached rep = $rep")
        }
        val data = generator.generate(num_obs)
        val dim_indices = data(0).indices.toSet
        val preprocessed = new RankIndex(data)
        val mc_vec = ksp.contrast_random_ref_dim_vec(preprocessed,dim_indices,MC_num)
        val to_write = (List(generator.id,rep) ++ mc_vec).mkString(",")
        summary.direct_write(summaryPath, to_write)
      }
    }
  }
}
