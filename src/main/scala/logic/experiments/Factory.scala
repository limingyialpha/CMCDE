package logic.experiments

import com.typesafe.scalalogging.LazyLogging
import logic.experiments.convergence.ConvDEAndACDistr
import logic.experiments.mcdepaper.Cor12Iteration
import logic.experiments.power.{CCPowerM, CPowerDE, CPowerST, GC3PowerM}
import logic.experiments.scalability.Scalability
import logic.experiments.sanity.Sanity

object Factory extends LazyLogging {

  val experiments_dictionary: Map[String, String => Experiment] = Map(
    "ConvDEAndACDistr" -> ConvDEAndACDistr,
    "Cor12Iteration" -> Cor12Iteration,
    "CPowerST" -> CPowerST,
    "CPowerDE" -> CPowerDE,
    "CCPowerM" -> CCPowerM,
    "GC3PowerM" -> GC3PowerM,
    "Sanity" -> Sanity,
    "Scalability" -> Scalability
  )

  def run(experiment: String, output_folder: String): Unit = {
    if (experiment == "all") {
      val exp_names = experiments_dictionary.keys.toVector
      logger.info("Now running all the experiments:")
      logger.info(s"${exp_names mkString ","}")
      for (name <- exp_names) {
        val exp = experiments_dictionary(name)
        exp(output_folder).run()
      }
      logger.info("Finished all experiments.")
    } else if (!experiments_dictionary.keys.toSet.contains(experiment)) {
      throw new RuntimeException("Wrong experiment name!")
    } else {
      experiments_dictionary(experiment)(output_folder).run()
    }
  }
}
