package logic.experiments.scalability

import logic.experiments.Experiment

object Scalability extends Experiment {
  def run(): Unit = {
    CCScalabilityD.run()
    CCScalabilityO.run()
    GC3ScalabilityD.run()
    GC3ScalabilityO.run()
  }
}
