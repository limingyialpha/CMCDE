package logic.experiments

object empty extends Experiment {

  def run() = {
    info("now running empty experiment")
    val attributes = List("genId", "technique", "rep", "alpha", "MC_num")
    val summary = ExperimentSummary(attributes)
    summary.write(summaryPath)
  }
}
