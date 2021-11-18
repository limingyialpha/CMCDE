package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, HyperSphere, Independent, Linear, Star}
import logic.utils.StopWatch
import logic.generators.{IndependentLinearRandomStripe, IndependentLinearStripe}
import logic.index.RankIndex
import logic.stats.mcde.KSP

object HeadTailBalanceTest extends Experiment {

  val alpha = 0.5
  val observation_num = 1000
  val generators: Vector[DataGenerator] = Vector(
    IndependentLinearRandomStripe(5,0.2,"gaussian",0)(Some(8))
  )

  var generated_data_points: Map[Array[Array[Double]], String] = Map(
  )

  for (generator <- generators) {
    generated_data_points += (generator.generate(observation_num) -> generator.id)
  }

  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)
  val estimator_shortnames = Set("ItGR", "ItGI","ItGIbV")

  val nrep = 1000

  val minimal_MC_num = 50

  def run():Unit = {
    info(s"Starting experiments")
    val attributes = List("genId", "estimator", "MC_num", "rep", "contrast","pre_cpu_ms","pre_wall_ms","contrast_cpu_ms", "contrast_wall_ms")
    val summary = ExperimentSummary(attributes)
    for {
      data <- (generated_data_points.keys).par
    } {
      val dims= data(0).indices.toSet
      val gen_id = generated_data_points(data)

      val preprocessed_values = StopWatch.measureTime(new RankIndex(data))
      val preprocessing_CPU_time = preprocessed_values._1
      val preprocessing_WALL_time = preprocessed_values._2
      val preprocessed = preprocessed_values._3

      // calculate the maximal MC_num of interest
      val dim_num = data(0).length
      val maximal_MC_num = (dim_num*3).max(minimal_MC_num)

      for{
        rep <- (1 to nrep).par
      }{
        if(rep % (nrep/10) == 0) {
          info(s"Generator id: ${gen_id}, Reached rep = $rep")
        }
        for {
          num_MC <- (1 to maximal_MC_num).par
        }{
          for (shortname <- estimator_shortnames){
            val contrast_values = StopWatch.measureTime(ksp.contrast(preprocessed,dims, num_MC)(shortname))
            val contrast_CPU_time = contrast_values._1
            val contrast_WALL_time = contrast_values._2
            val contrast = contrast_values._3
            val to_write= List(gen_id,shortname,num_MC,rep,contrast,preprocessing_CPU_time,preprocessing_WALL_time,contrast_CPU_time,contrast_WALL_time).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
      }
    }
  }
}
