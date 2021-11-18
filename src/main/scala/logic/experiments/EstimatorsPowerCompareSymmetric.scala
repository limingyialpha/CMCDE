package logic.experiments

import breeze.stats.DescriptiveStats.percentile
import io.github.edouardfouche.generators.{Cross, DataGenerator, DoubleLinear, Hourglass, HyperSphere, Hypercube, HypercubeGraph, Independent, Linear, Parabola, Sine, Star, Zinv}
import logic.index.RankIndex
import logic.stats.mcde.KSP
import logic.data.Utility.round

object EstimatorsPowerCompareSymmetric extends Experiment {
  val dimensions_of_interest = Vector(2,3,5,10,20)
  val noise_levels = 30
  val noises = (0 to noise_levels).toArray.map(x => round(x.toDouble / noise_levels.toDouble, 2))
  //val estimators = Vector("R")
  val estimators = Vector("R", "ItGI")
  val MC_num = 50
  val obs_num = 1000

  val independent_benchmark_iteration_num = 1000

  val comparison_iteration_num = 500
  val generators: Vector[(Int, Double, String, Int) => DataGenerator] = Vector(
    Linear,
    Cross,
    DoubleLinear(_:Int,_:Double,_:String,_:Int)(Some(0.25)),
    Hourglass,
    Hypercube,
    HypercubeGraph,
    HyperSphere,
    Parabola(_:Int,_:Double,_:String,_:Int)(scale=Some(1)),
    Sine(_:Int,_:Double,_:String,_:Int)(period = Some(1)),
    Sine(_:Int,_:Double,_:String,_:Int)(period = Some(5)),
    Star,
    Zinv,
  )

  val ksp = KSP(MC_num, alpha = 0.5, parallelize = 1)

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting experiments - ${this.getClass.getSimpleName}")
    info(s"Parameters:")
    info(s"dimension of interest: ${dimensions_of_interest mkString ","}")
    info(s"noise_levels: ${noise_levels}")
    info(s"number of observations: ${obs_num}")
    info(s"estimators: ${estimators mkString ","}")
    info(s"number of MC iterations: ${MC_num}")
    info(s"number of independent benchmark repetitions: ${independent_benchmark_iteration_num}")
    info(s"number of repetitions for comparison: ${comparison_iteration_num}")
    info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

    val attributes = List("genId", "dim", "noise", "estimator", "power")
    val summary = ExperimentSummary(attributes)
    for (estimator <- estimators) {
      for (dim <- dimensions_of_interest) {
        info(s"Now handling estimator ${estimator}, dimensions ${dim}")
        val independent_benchmark_instance = Independent(dim, 0, "gaussian", 0)
        val independent_benchmark_contrasts = (1 to independent_benchmark_iteration_num).par.map(_ => {
          val data = independent_benchmark_instance.generate(obs_num)
          val indices_set = data(0).indices.toSet
          val preprocessed = new RankIndex(data)
          ksp.contrast(preprocessed, indices_set)(estimator)
        }).toVector
        val threshold95 = percentile(independent_benchmark_contrasts, 0.95)

        for (noise <- noises.par) {
          for (gen <- generators.par) {
            val generator_instance = gen(dim, noise, "gaussian", 0)
            val comparison_contrasts = (1 to comparison_iteration_num).par.map(_ => {
              val data = generator_instance.generate(obs_num)
              val indices_set = data(0).indices.toSet
              val preprocessed = new RankIndex(data)
              ksp.contrast(preprocessed, indices_set)(estimator)
            }).toVector
            val power = comparison_contrasts.count(c => (c > threshold95)).toDouble / comparison_iteration_num.toDouble
            val to_write = List(generator_instance.id, dim, noise, estimator, power).mkString(",")
            summary.direct_write(summaryPath, to_write)
          }
        }
      }
    }
  }
}
