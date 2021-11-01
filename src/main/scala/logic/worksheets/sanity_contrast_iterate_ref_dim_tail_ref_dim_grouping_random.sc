import logic.data.DataCombiner
import logic.index.RankIndex
import logic.stats.mcde.KSP
import io.github.edouardfouche.generators.Independent
import io.github.edouardfouche.generators.Linear
import logic.generators.IndependentLinearIterate


//val data = DataCombiner.generate_combine(1000,"i",5,0,"l",5,0.1)
val data = IndependentLinearIterate(10,0.0,"gaussian",0).generate(1000)
val num_dims = data(0).indices.toSet
val preprocessed = new RankIndex(data)
val ksp = KSP(parallelize = 0)
val MC_num = 12
val contrast = ksp.contrast_iterate_ref_dim_tail_ref_dim_grouping_by_influence(preprocessed,num_dims,MC_num = MC_num)
