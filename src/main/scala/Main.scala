import logic.experiments.Factory

object Main {
  def main(args: Array[String]): Unit = {
    val experiment = args(0) //names or "all"
    val output_folder = args(1)
    Factory.run(experiment, output_folder)
  }
}
