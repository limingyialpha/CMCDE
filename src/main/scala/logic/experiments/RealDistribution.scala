package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, Independent, Linear}
import logic.experiments.ProvePaperWrong.{info, summaryPath}
import logic.index.RankIndex
import logic.stats.mcde.KSP


/**
 * This is a sister experiment of ProvePaperWrong
 * After we see, that looking at the distribution of approximated
 * contrast with 1 MC iterations does not imply the power of the estimator.
 * We need the approximated contrast distribution with M = 50. This number
 * is the default in the paper
 *
 * The parameters should be the same as in the ProvePaperWrong
 * 1. generator is Independent or Linear with gaussian noise level 0.4
 * 2. 10000 iterations
 * 3. 1000 observations in each iteration
 * 4. MC iteration technique is choosing random reference dimension
 * 5. alpha 0.5
 *
 */
object RealDistribution extends Experiment {
  val alpha = 0.5
  val generators: Vector[DataGenerator] = Vector(
    Independent(2,0,"gaussian", 0),
    Linear(2,0.4,"gaussian",0)
  )
  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)
  val nrep = 10000
  val num_obs = 1000
  val MC_num = 50

  def run() = {
    info(s"Starting experiments")
    val attributes = List("genId", "rep", "contrast")
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
        val contrast = ksp.contrast_random_ref_dim(preprocessed,dim_indices,MC_num)("c")
        summary.add("genID", generator.id)
        summary.add("rep",rep)
        summary.add("contrast", contrast)
        summary.write(summaryPath)
      }
    }
  }
}
