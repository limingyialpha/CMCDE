import logic.experiments.convergence.ConvDEAndACDistr
import logic.experiments.mcdepaper.Cor12Iteration
import logic.experiments.power.{CCPowerM, CPowerDE, CPowerST, GC3PowerM}
import logic.experiments.scalability.Scalability
import logic.experiments.sanity.Sanity

object Main {
  def main(args: Array[String]): Unit = {
    GC3PowerM.run()
    CCPowerM.run()
    CPowerDE.run()
    CPowerST.run()
  }
}
