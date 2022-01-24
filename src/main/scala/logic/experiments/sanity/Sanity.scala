package logic.experiments.sanity

import logic.experiments.Experiment

case class Sanity(output_folder: String) extends Experiment(output_folder) {
  def run(): Unit = {
    Dilute(output_folder).run()
    Duplicate(output_folder).run()
  }
}
