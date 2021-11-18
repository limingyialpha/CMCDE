package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, HyperSphere, Independent, Linear}
import logic.data.DataCombiner
import logic.experiments.TailHandlingCompare.observation_num
import logic.generators.{IndependentLinearRandomStripe, IndependentLinearStripe}
import logic.index.{Index, RankIndex}
import logic.stats.mcde.{KSP, McdeStats}

object SliceTechniquesCompare extends Experiment {
  type PreprocessedData = RankIndex
  val alpha = 0.5
  val observation_num = 1000

  val generators: Vector[DataGenerator] = Vector(
    Independent(2,0,"gaussian", 0),
    Independent(10,0,"gaussian", 0),
    Independent(20,0,"gaussian", 0),
    Linear(2,0.4,"gaussian",0),
    Linear(10,0.4,"gaussian",0),
    Linear(20,0.4,"gaussian",0),
    IndependentLinearStripe(2,0.2,"gaussian",0),
    IndependentLinearStripe(10,0.2,"gaussian",0),
    IndependentLinearStripe(20,0.2,"gaussian",0),
    IndependentLinearRandomStripe(10,0.2,"gaussian",0)(Some(3)),
    IndependentLinearRandomStripe(20,0.2,"gaussian",0)(Some(6)),
    IndependentLinearRandomStripe(10,0.2,"gaussian",0)(Some(7)),
    IndependentLinearRandomStripe(20,0.2,"gaussian",0)(Some(14))
  )

  var generated_data_points: Map[Array[Array[Double]], String] = Map(
    DataCombiner.generate_combine(observation_num,"i",5,0,"l",5,0.1) -> "independent-5-0-linear-5-0.1",
    DataCombiner.generate_combine(observation_num,"i",10,0,"l",10,0.1) -> "independent-10-0-linear-10-0.1",
  )

  for (generator <- generators) {
    generated_data_points += (generator.generate(observation_num) -> generator.id)
  }

  //1 .  val ksp = val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)
  val ksp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)

  //for testing purpose
  //  val nrep = 10
  //  val MC_num = 7

  val nrep = 1000
  val MC_num = 50

  def run():Unit = {
  }
}
