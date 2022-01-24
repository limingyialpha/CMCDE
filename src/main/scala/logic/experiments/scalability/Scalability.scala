package logic.experiments.scalability

import logic.experiments.Experiment

case class Scalability(output_folder: String) extends Experiment(output_folder) {
  def run(): Unit = {
    CCScalabilityD(output_folder).run()
    CCScalabilityO(output_folder).run()
    GC3ScalabilityD(output_folder).run()
    GC3ScalabilityO(output_folder).run()
  }
}
