import io.github.edouardfouche.generators.Independent
import logic.index.RankIndex
import logic.stats.mcde.KSP

val independent_generator = Independent(4, 0.0, "gaussian", 0)
val raw_data = independent_generator.generate(1000)
val preprocessed = new RankIndex(raw_data)
val ksp = KSP(M = 10000, alpha = 0.5, parallelize = 1)

val influence_012_3 = ksp.influence(preprocessed,Set(0,1,2),Set(3))