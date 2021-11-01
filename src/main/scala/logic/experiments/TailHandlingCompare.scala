package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, HyperSphere, Independent, Linear}
import logic.experiments.ContrastApproximationConvergenceSpeed.summaryPath
import logic.generators.IndependentLinearIterate
import logic.index.RankIndex
import logic.stats.mcde.KSP

object TailHandlingCompare extends Experiment {

  val alpha = 0.5
  val observation_num = 1000
  val generators: Vector[DataGenerator] = Vector(
    IndependentLinearIterate(30,0.1,"gaussian",0)
  )

  var generated_data_points: Map[Array[Array[Double]], String] = Map(
    //DataCombiner.generate_combine(observation_num,"i",50,0,"l",50,0.1) -> "independent-50-0-linear-50-0.1"
  )

  for (generator <- generators) {
    generated_data_points += (generator.generate(observation_num) -> generator.id)
  }

  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)

  val nrep = 1000
  val upper_MC_num = 100

  def run():Unit = {
    info(s"Starting experiments")
    val attributes = List("genId", "MC_technique", "MC_num", "rep", "contrast")
    val summary = ExperimentSummary(attributes)
    for {
      data <- generated_data_points.keys
    } {
      val dim_indices = data(0).indices.toSet
      val gen_id = generated_data_points(data)

      // Save data samples (debugging purpose)
      utils.createFolderIfNotExisting(experiment_folder + "/data")
      utils.saveDataSet(data, experiment_folder + "/data/" + s"${gen_id}")
      val preprocessed = new RankIndex(data)

      for{
        rep <- (1 to nrep).par
      }{
        if(rep % (nrep/10) == 0) {
          info(s"Generator id: ${gen_id}, Reached rep = $rep")
        }
        for {
          num_MC <- (1 to upper_MC_num).par
        }{
          val contrast_iterate_MC = ksp.contrast_iterate_ref_dim_tail_MC(preprocessed, dim_indices, num_MC)
          val contrast_iterate_group_random = ksp.contrast_iterate_ref_dim_tail_ref_dim_grouping_random(preprocessed,dim_indices,num_MC)
          val contrast_iterate_group_influence = ksp.contrast_iterate_ref_dim_tail_ref_dim_grouping_by_influence(preprocessed,dim_indices,num_MC)
          val to_write_contrast_iterate_MC = List(gen_id,"iterate_tail_MC",num_MC,rep, contrast_iterate_MC).mkString(",")
          val to_write_contrast_iterate_group_random = List(gen_id,"iterate_tail_group_random",num_MC,rep, contrast_iterate_group_random).mkString(",")
          val to_write_contrast_iterate_group_influence = List(gen_id,"iterate_tail_group_by_influence",num_MC,rep, contrast_iterate_group_influence).mkString(",")
          summary.direct_write(summaryPath, to_write_contrast_iterate_MC)
          summary.direct_write(summaryPath,to_write_contrast_iterate_group_random)
          summary.direct_write(summaryPath,to_write_contrast_iterate_group_influence)
        }
      }
    }
  }
}
