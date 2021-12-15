import logic.experiments.convergence.ConvDEAndACDistr
import logic.experiments.mcdepaper.Cor12Iteration
import logic.experiments.power.{CCPowerM, CPowerDE, CPowerST, GC3PowerM}
import logic.experiments.scalability.Scalability
import logic.experiments.sanity.Sanity

object Main {
  def main(args: Array[String]): Unit = {
    val output_folder = args(0)
    new CCPowerM(output_folder).run()
    new GC3PowerM(output_folder).run()
    new CPowerST(output_folder).run()
    new CPowerDE(output_folder).run()
  }
}
