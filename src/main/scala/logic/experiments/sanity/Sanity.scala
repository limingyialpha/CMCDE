package logic.experiments.sanity

import logic.experiments.Experiment

object Sanity extends Experiment {
  def run(): Unit = {
    Dilute.run()
    Duplicate.run()
  }
}
