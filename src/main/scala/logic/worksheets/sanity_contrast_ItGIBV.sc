import io.github.edouardfouche.generators.{Independent, Linear}
import logic.index.RankIndex
import logic.stats.mcde.KSP

val gen1 = Independent(10,0.0,"gaussian",0)
val gen2 = Linear(10,0.0,"gaussian",0)
val data1 = gen1.generate(1000)
val data2 = gen2.generate(1000)
val preprocessed1 = new RankIndex(data1)
val dims1 = data1(0).indices.toSet
val preprocessed2 = new RankIndex(data2)
val dims2 = data2(0).indices.toSet

val ksp = KSP(M = 50, alpha = 0.5, parallelize = 1)

val MC_num = 95
ksp.contrast_iterate_ref_dim_tail_group_ref_dim_by_influence_balance_with_variance(preprocessed2,dims2,MC_num)("c")
