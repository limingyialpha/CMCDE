package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, HyperSphere, Independent, Linear, Star}
import logic.data.DataCombiner
import logic.utils.StopWatch
import logic.generators.{IndependentLinearRandomStripe, IndependentLinearStripe}
import logic.index.RankIndex
import logic.stats.mcde.KSP

object TailHandlingCompare extends Experiment {

  val alpha = 0.5
  val observation_num = 1000
  val generators: Vector[DataGenerator] = Vector(
    Independent(10,0,"gaussian", 0),
    Independent(30,0,"gaussian", 0),
    Linear(10,0.4,"gaussian",0),
    Linear(30,0.4,"gaussian",0),
    IndependentLinearStripe(10,0.2,"gaussian",0),
    IndependentLinearStripe(30,0.2,"gaussian",0),
    IndependentLinearRandomStripe(10,0.2,"gaussian",0)(Some(3)),
    IndependentLinearRandomStripe(30,0.2,"gaussian",0)(Some(9)),
    IndependentLinearRandomStripe(10,0.2,"gaussian",0)(Some(7)),
    IndependentLinearRandomStripe(30,0.2,"gaussian",0)(Some(21))
  )

  var generated_data_points: Map[Array[Array[Double]], String] = Map(
    DataCombiner.generate_combine(observation_num,"i",5,0,"l",5,0.1) -> "independent-5-0-linear-5-0.1",
    DataCombiner.generate_combine(observation_num,"i",15,0,"l",15,0.1) -> "independent-15-0-linear-15-0.1",
  )

  for (generator <- generators) {
    generated_data_points += (generator.generate(observation_num) -> generator.id)
  }

  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)

  val nrep = 1000

  val minimal_MC_num = 50

  def run():Unit = {
    info(s"Starting experiments")
    val attributes = List("genId", "MC_technique", "MC_num", "rep", "contrast","pre_cpu_ms","pre_wall_ms","contrast_cpu_ms", "contrast_wall_ms")
    val summary = ExperimentSummary(attributes)
    for {
      data <- (generated_data_points.keys).par
    } {
      val dims_vec = data(0).indices.toVector
      val dims_set = data(0).indices.toSet
      val gen_id = generated_data_points(data)

      // Save data samples (debugging purpose)
      utils.createFolderIfNotExisting(experiment_folder + "/data")
      utils.saveDataSet(data, experiment_folder + "/data/" + s"${gen_id}")

      val preprocessed_values = StopWatch.measureTime(new RankIndex(data))
      val preprocessing_CPU_time = preprocessed_values._1
      val preprocessing_WALL_time = preprocessed_values._2
      val preprocessed = preprocessed_values._3

      // calculate the maximal MC_num of interest
      val dim_num = data(0).length
      val maximal_MC_num = (dim_num*3).max(minimal_MC_num)

      for{
        rep <- (1 to nrep)
      }{
        if(rep % (nrep/10) == 0) {
          info(s"Generator id: ${gen_id}, Reached rep = $rep")
        }
        for {
          num_MC <- (1 to maximal_MC_num)
        }{
          for (shortname <- ksp.contrast_method_shortnames){
            val contrast_values = StopWatch.measureTime(ksp.contrast_by_shortname(preprocessed,dims_vec, num_MC, shortname))
            val contrast_CPU_time = contrast_values._1
            val contrast_WALL_time = contrast_values._2
            val contrast = preprocessed_values._3
            val to_write= List(gen_id,shortname,num_MC,rep,contrast,preprocessing_CPU_time,preprocessing_WALL_time,contrast_CPU_time,contrast_WALL_time).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
      }
    }
  }
}
