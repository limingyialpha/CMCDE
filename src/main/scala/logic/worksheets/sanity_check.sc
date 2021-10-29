import io.github.edouardfouche.generators.{Independent,Linear}
import logic.index.RankIndex
import logic.stats.mcde.KSP
import logic.data.DataCombiner

val independent_generator = Independent(50, 0.0, "gaussian", 0)
val raw_data = independent_generator.generate(1000)
val preprocessed = new RankIndex(raw_data)
val ksp = KSP(M = 200, alpha = 0.5, parallelize = 1)

val influence = ksp.influence(preprocessed, Set(0,1),Set(2,3))
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, raw_data(0).indices.toSet)
val contrast_random = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet)
val canonical_contrast = ksp.canonical_contrast_pair(preprocessed,(0 to 24).toSet,(25 to 49).toSet)

// linear noise 0.4

val linear_generator = Linear(50, 0.4, "gaussian", 0)
val raw_data = linear_generator.generate(1000)
val preprocessed = new RankIndex(raw_data)
val ksp = KSP(M = 200, alpha = 0.5, parallelize = 1)

val influence = ksp.influence(preprocessed, Set(0,1),Set(2,3))
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, raw_data(0).indices.toSet)
val canonical_contrast = ksp.canonical_contrast_pair(preprocessed,(0 to 2).toSet,(3 to 49).toSet)

//combined data 2dim ind 2dim linear

val combined_raw_data = DataCombiner.generate_combine(10000,"i",2,0,"l",2,0)
val preprocessed = new RankIndex(combined_raw_data)
val ksp = KSP(M = 1000, alpha = 0.5, parallelize = 1)

val influence = ksp.influence(preprocessed, Set(0,1),Set(2,3))
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, combined_raw_data(0).indices.toSet)
val canonical_contrast = ksp.canonical_contrast_pair(preprocessed,Set(0,1),Set(2,3))

val influence = ksp.influence(preprocessed, Set(1),Set(2,3))
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, combined_raw_data(0).indices.toSet)
val canonical_contrast = ksp.canonical_contrast_pair(preprocessed,Set(0),Set(2,3))

val influence = ksp.influence(preprocessed, Set(0,2),Set(1,3))
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, Set(2,3))
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, Set(0,1))
val canonical_contrast = ksp.canonical_contrast_pair(preprocessed,Set(0,2),Set(1,3))


// all the values seem sane