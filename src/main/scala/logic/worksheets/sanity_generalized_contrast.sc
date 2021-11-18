import logic.data.DataCombiner
import logic.index.RankIndex
import logic.stats.mcde.KSP

val data = DataCombiner.generate_combine(1000,"l",2,0.0,"l",8,0.0)
val preprocessed = new RankIndex(data)
val ksp = KSP(50,0.5,1)
val groups_of_dimensions = Set(Set(0), Set(1))
