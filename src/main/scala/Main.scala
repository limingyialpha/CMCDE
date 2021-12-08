import logic.experiments.convergence.ConvDEAndACDistr
import logic.experiments.mcdepaper.Cor12Iteration
import logic.experiments.power.{CCPowerM, CPowerDE, CPowerST, GC3PowerM}
import logic.experiments.scalability.{CCScalabilityD, CCScalabilityO, GC3ScalabilityD, GC3ScalabilityO}

object Main {
  def main(args: Array[String]): Unit = {
//    CPowerDE.run()
//    CPowerST.run()
//    CCPowerM.run()
//    GD3PowerM.run()
//    CCScalabilityD.run()
//    CCScalabilityO.run()
    GC3ScalabilityD.run()
    GC3ScalabilityO.run()
    GC3PowerM.run()
//    ConvDEAndACDistr.run()
  }
}
