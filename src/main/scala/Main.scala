import logic.experiments.convergence.ConvDEAndACDistr
import logic.experiments.power.{CCPowerM, CPowerDE, CPowerST, GD3PowerM}
import logic.experiments.scalability.{CCScalabilityD, CCScalabilityO, GD3ScalabilityD, GD3ScalabilityO}

object Main {
  def main(args: Array[String]): Unit = {
    CPowerDE.run()
    CPowerST.run()
    CCPowerM.run()
    GD3PowerM.run()
    CCScalabilityD.run()
    CCScalabilityO.run()
    GD3ScalabilityD.run()
    GD3ScalabilityO.run()
    ConvDEAndACDistr.run()
  }
}
