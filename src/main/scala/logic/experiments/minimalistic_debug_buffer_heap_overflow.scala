package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, HyperSphere, Independent, Linear}
import logic.index.{Index, RankIndex}
import logic.stats.mcde.{KSP, McdeStats}

object minimalistic_debug_buffer_heap_overflow extends Experiment {
  type PreprocessedData = RankIndex

  val alpha = 0.5
  val observation_num = 1000
  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 0)


  val nrep = 1000
  val MC_num = 1500

  def run():Unit = {
    info(s"Starting experiments")
    val generator = Independent(2, 0, "gaussian", 0)
    val data = generator.generate(observation_num)
    val dim_indices = data(0).indices.toSet
    val preprocessed = new RankIndex(data)
    for {rep <- 1 to nrep} {
      info(s"Generator id: ${generator.id}, Reached rep = $rep")
      val iterate_array = ksp.contrast_iterate_ref_dim_cumulative_average_vec(preprocessed, dim_indices, MC_num = MC_num)
      //val iterate_uniform_array = ksp.contrast_iterate_uniform_cumulative_average_vec(preprocessed,dim_indices,MC_num)
    }
  }
}
