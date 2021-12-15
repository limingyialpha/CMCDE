package logic.experiments.sanity

import logic.experiments.Experiment

class Sanity(output_folder: String) extends Experiment(output_folder) {
  def run(): Unit = {
    new Dilute(output_folder).run()
    new Duplicate(output_folder).run()
  }
}
