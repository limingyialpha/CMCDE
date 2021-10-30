package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, HyperSphere, Independent, Linear}
import logic.data.DataCombiner
import logic.index.{Index, RankIndex}
import logic.stats.mcde.{KSP, McdeStats}

import scala.collection.parallel.ForkJoinTaskSupport


/**
 * In this experiment, we generate n set of m data points
 *
 * We compare the convergence speed to the contrast on each set
 * with the following 2 MC iteration techniques:
 * 1. choosing random reference dimensions
 * 2. iterating through reference dimensions
 *
 * The process is:
 * we iterate through each generator, generate adequate number data points
 * save the data set to a file. For both techniques, we generate rep times
 * of cumulative average arrays and save each one.
 */
object iterate_vs_uniform_iterate_conv_speed extends Experiment {
  type PreprocessedData = RankIndex

  val alpha = 0.5
  val observation_num = 1000
  val generators: Vector[DataGenerator] = Vector(
    Independent(2,0,"gaussian", 0),
    Independent(50,0,"gaussian", 0),
    Independent(100,0,"gaussian", 0),
    Linear(2,0.4,"gaussian",0),
    Linear(50,0.4,"gaussian",0),
    Linear(100,0.4,"gaussian",0),
    HyperSphere(2,0.2,"gaussian",0),
    HyperSphere(50,0.2,"gaussian",0),
    HyperSphere(100,0.2,"gaussian",0)
  )

  val combined_data_points: Map[Array[Array[Double]], String] = Map(
    DataCombiner.generate_combine(observation_num,"i",2,0,"l",2,0.1) -> "independent-2-0-linear-2-0.1",
    DataCombiner.generate_combine(observation_num,"i",50,0,"l",50,0.1) -> "independent-50-0-linear-50-0.1",
    DataCombiner.generate_combine(observation_num,"i",100,0,"l",100,0.1) -> "independent-100-0-linear-100-0.1",
    DataCombiner.generate_combine(observation_num,"i",2,0,"l",50,0.1) -> "independent-2-0-linear-50-0.1",
    DataCombiner.generate_combine(observation_num,"i",50,0,"l",2,0.1) -> "independent-50-0-linear-2-0.1"
  )

  //1 .  val ksp = val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)
  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 0)

  //for testing purpose
  //  val nrep = 10
  //  val MC_num = 7

  val nrep = 500
  val MC_num = 1000

  def run():Unit = {
    info(s"Starting experiments")
    val attributes = List("genId", "technique", "rep") ++ (1 to MC_num).map(x => x.toString).toList
    val summary = ExperimentSummary(attributes)
     // dealing with generators
    for {
      // 1. generator <- generators
      generator <- generators.par
    } {
      // TODO it remains a question if 1000 observations are adequate for all number of dimensions
      val data = generator.generate(observation_num)
      val dim_indices = data(0).indices.toSet
      // Save data samples (debugging purpose)
      // 2. do not save data
      utils.createFolderIfNotExisting(experiment_folder + "/data")
      utils.saveDataSet(data, experiment_folder + "/data/" + s"${generator.id}")
      val preprocessed = new RankIndex(data)
      for{rep <- 1 to nrep}{
        if(rep % (nrep/10) == 0) {
          info(s"Generator id: ${generator.id}, Reached rep = $rep")
        }
        val iterate_array = ksp.contrast_iterate_ref_dim_cumulative_average_vec(preprocessed,dim_indices, MC_num = MC_num)
        val iterate_to_write = (List(generator.id,"iterate",rep) ++ iterate_array).mkString(",")
        summary.direct_write(summaryPath,iterate_to_write)
        val iterate_uniform_array = ksp.contrast_iterate_uniform_cumulative_average_vec(preprocessed,dim_indices,MC_num)
        val iterate_uniform_to_write = (List(generator.id,"iterate_uniform",rep) ++ iterate_uniform_array).mkString(",")
        summary.direct_write(summaryPath,iterate_uniform_to_write)
      }
    }
    // dealing with combined data
    for {
      combined_data <- (combined_data_points.keys).par
      //combined_data <- combined_data_points.keys
    } {
      // TODO it remains a question if 1000 observations are adequate for all number of dimensions
      val data = combined_data
      val dim_indices = data(0).indices.toSet
      val gen_id = combined_data_points(combined_data)
      // Save data samples (debugging purpose)
      // 2. do not save data
      utils.createFolderIfNotExisting(experiment_folder + "/data")
      utils.saveDataSet(data, experiment_folder + "/data/" + s"${gen_id}")
      val preprocessed = new RankIndex(data)
      for{rep <- 1 to nrep}{
        if(rep % (nrep/10) == 0) {
          info(s"Generator id: ${gen_id}, Reached rep = $rep")
        }
        val iterate_array = ksp.contrast_iterate_ref_dim_cumulative_average_vec(preprocessed,dim_indices, MC_num = MC_num)
        val iterate_to_write = (List(gen_id,"iterate",rep) ++ iterate_array).mkString(",")
        summary.direct_write(summaryPath,iterate_to_write)
        val iterate_uniform_array = ksp.contrast_iterate_uniform_cumulative_average_vec(preprocessed,dim_indices,MC_num)
        val iterate_uniform_to_write = (List(gen_id,"iterate_uniform",rep) ++ iterate_uniform_array).mkString(",")
        summary.direct_write(summaryPath,iterate_uniform_to_write)
      }
    }
  }
}
