package logic.experiments

import com.typesafe.scalalogging.LazyLogging
import logic.experiments.convergence.ConvDE
import logic.experiments.distribution.{DistrAC, DistrIV}
import logic.experiments.mcdepaper.Cor12Iteration
import logic.experiments.power.{CCPowerM, CPowerDE, CPowerST, GC3PowerM}
import logic.experiments.scalability.Scalability
import logic.experiments.sanity.Sanity

object Factory extends LazyLogging {

  val experiments_names = Vector(
    "Sanity",
    "Scalability",
    "Cor12Iteration",
    "ConvDE",
    "DistrAC",
    "DistrIV",
    "CPowerST",
    "CPowerDE",
    "CCPowerM",
    "GC3PowerM"
  )

  val experiments_dictionary: Map[String, String => Experiment] = Map(
    "Sanity" -> Sanity,
    "Scalability" -> Scalability,
    "Cor12Iteration" -> Cor12Iteration,
    "ConvDE" -> ConvDE,
    "DistrAC" -> DistrAC,
    "DistrIV" -> DistrIV,
    "CPowerST" -> CPowerST,
    "CPowerDE" -> CPowerDE,
    "CCPowerM" -> CCPowerM,
    "GC3PowerM" -> GC3PowerM
  )

  def run(experiments: List[String], output_folder: String): Unit = {
    if (experiments.isEmpty) {
      throw new RuntimeException("No experiments given!")
    } else if (experiments.size == 1 & experiments.head == "all") {
      logger.info("Now running ALL experiments:")
      logger.info(s"${experiments_names mkString ","}")
      for (name <- experiments_names) {
        val exp = experiments_dictionary(name)
        exp(output_folder).run()
      }
      logger.info("Finished ALL experiments.")
    } else {
      val correct = experiments.forall(e => experiments_names.contains(e))
      if (correct) {
        for (e <- experiments) {
          experiments_dictionary(e)(output_folder).run()
        }
      } else throw new RuntimeException("Wrong experiment name (names)!")
    }
  }
}
