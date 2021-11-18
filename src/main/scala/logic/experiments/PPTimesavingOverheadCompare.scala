package logic.experiments

import io.github.edouardfouche.generators.{DataGenerator, Independent, Linear}
import logic.generators.{IndependentLinearRandomStripe, IndependentLinearStripe}
import logic.index.RankIndex
import logic.stats.mcde.KSP
import logic.utils.StopWatch

/**
 * Conclusion:
 * In future experiments, we should
 * activate all parallel programming parts.
 * The time saving overweights the overhead
 */
object PPTimesavingOverheadCompare extends Experiment {

  val alpha = 0.5
  val observation_num = 1000
  val generators: Vector[DataGenerator] = Vector(
    Independent(10,0,"gaussian", 0),
    Independent(30,0,"gaussian", 0),
    Linear(10,0.4,"gaussian",0),
    Linear(30,0.4,"gaussian",0),
    IndependentLinearStripe(10,0.2,"gaussian",0),
    IndependentLinearStripe(30,0.2,"gaussian",0)
  )
  val MC_num_upper = 20


  val ksp_no_pp: KSP = KSP(M = 50, alpha = alpha, parallelize = 0)
  val ksp_with_pp: KSP = KSP(M = 50, alpha = alpha, parallelize = 1)
  val nrep = 60

  def run(): Unit = {
    info(s"Starting experiments")

    def measure_time(gen_PP: Boolean, nrep_PP: Boolean, ksp_PP: Boolean, MC_PP: Boolean): Unit = {
      val gen_it = if (gen_PP) generators.par else generators
      val ksp = if (ksp_PP) ksp_with_pp else ksp_no_pp
      val times = StopWatch.measureTime({
        for {gen <- gen_it} {
          val data = gen.generate(observation_num)
          val preprocessed = new RankIndex(data)
          val dims = data(0).indices.toSet
          val nrep_it = if (nrep_PP) (1 to nrep).par else (1 to nrep)
          for {rep <- nrep_it} {
            val MC_it = if (MC_PP) (1 to MC_num_upper).par else (1 to MC_num_upper)
            for {mc <- MC_it}
            ksp.contrast(preprocessed, dims, mc)("ItR", "c")
          }
        }
      })
      println(s"generator PP: $gen_PP, nrep PP: $nrep_PP, ksp PP: $ksp_PP, MC PP: $MC_PP")
      println(s"CPU time: ${times._1}")
      println(s"WALL time  ${times._2}")
    }

    // all true
    measure_time(true,true,true, true)

    // 3 true
    measure_time(true,true,true, false)
    measure_time(true,false,true, true)
    measure_time(true,true,false, true)
    measure_time(false, true, true,true)

    // 2 true
    measure_time(true,true,false, false)
    measure_time(false,true,true, false)
    measure_time(false,false,true, true)
    measure_time(false,true,false, true)
    measure_time(true,false,true, false)
    measure_time(true,false,false, true)


    // second run
    // all true
    measure_time(true,true,true, true)

    // 3 true
    measure_time(true,true,true, false)
    measure_time(true,false,true, true)
    measure_time(true,true,false, true)
    measure_time(false, true, true,true)

    // 2 true
    measure_time(true,true,false, false)
    measure_time(false,true,true, false)
    measure_time(false,false,true, true)
    measure_time(false,true,false, true)
    measure_time(true,false,true, false)
    measure_time(true,false,false, true)
  }
}
