import logic.experiments.Factory

object Main {
  def main(args: Array[String]): Unit = {
    val output_folder = args(0)
    val experiments = args.drop(1).toList // lsit of names or "all"
    Factory.run(experiments, output_folder)
  }
}
