package logic.experiments.scalability

import logic.experiments.Experiment

class Scalability(output_folder: String) extends Experiment(output_folder) {
  def run(): Unit = {
    new CCScalabilityD(output_folder).run()
    new CCScalabilityO(output_folder).run()
    new GC3ScalabilityD(output_folder).run()
    new GC3ScalabilityO(output_folder).run()
  }
}
