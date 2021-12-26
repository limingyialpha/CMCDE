package logic.experiments

import com.typesafe.scalalogging.LazyLogging
import logic.experiments.convergence.ConvDEAndACDistr
import logic.experiments.mcdepaper.Cor12Iteration
import logic.experiments.power.{CCPowerM, CPowerDE, CPowerST, GC3PowerM}
import logic.experiments.scalability.Scalability
import logic.experiments.sanity.Sanity

object Factory extends LazyLogging {

  val experiments_names = Vector(
    "Sanity",
    "Scalability",
    "Cor12Iteration",
    "ConvDEAndACDistr",
    "CPowerST",
    "CPowerDE",
    "CCPowerM",
    "GC3PowerM"
  )

  val experiments_dictionary: Map[String, String => Experiment] = Map(
    "Sanity" -> Sanity,
    "Scalability" -> Scalability,
    "Cor12Iteration" -> Cor12Iteration,
    "ConvDEAndACDistr" -> ConvDEAndACDistr,
    "CPowerST" -> CPowerST,
    "CPowerDE" -> CPowerDE,
    "CCPowerM" -> CCPowerM,
    "GC3PowerM" -> GC3PowerM
  )

  def run(experiment: String, output_folder: String): Unit = {
    if (experiment == "all") {
      logger.info("Now running all the experiments:")
      logger.info(s"${experiments_names mkString ","}")
      for (name <- experiments_names) {
        val exp = experiments_dictionary(name)
        exp(output_folder).run()
      }
      logger.info("Finished all experiments.")
    } else if (!experiments_names.contains(experiment)) {
      throw new RuntimeException("Wrong experiment name!")
    } else {
      experiments_dictionary(experiment)(output_folder).run()
    }
  }
}
