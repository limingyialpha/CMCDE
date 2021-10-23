import io.github.edouardfouche.generators.{HyperSphere, Independent, Linear}
import logic.index.RankIndex
import logic.stats.mcde.KSP

val independent_generator = Independent(3, 0.0, "gaussian", 0)
val raw_data = independent_generator.generate(1000)
val preprocessed = new RankIndex(raw_data)
val ksp = KSP(M = 50, alpha = 0.5, parallelize = 1)

/// uniform iid data

//// we first get the benchmark by doing MC iteration with random reference number 100000 times
val contrast_random_benchmark = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 100000)
// then we compare the random ref dim and iterate ref dim
val contrast_random = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)

/// linear data noise = 0

val linear_generator = Linear(3, 0.0, "gaussian", 0)
val raw_data = linear_generator.generate(1000)
val preprocessed = new RankIndex(raw_data)
val ksp = KSP(M = 50, alpha = 0.5, parallelize = 1)

val contrast_random_benchmark = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 100000)
// then we compare the random ref dim and iterate ref dim
val contrast_random = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)

/// linear data noise = 0.4

val linear_generator = Linear(3, 0.4, "gaussian", 0)
val raw_data = linear_generator.generate(1000)
val preprocessed = new RankIndex(raw_data)
val ksp = KSP(M = 50, alpha = 0.5, parallelize = 1)

val contrast_random_benchmark = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 100000)
// then we compare the random ref dim and iterate ref dim
val contrast_random = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)

// hypersphere data noise = 0.3

val hs_generator = HyperSphere(3, 0.3, "gaussian", 0)
val raw_data = hs_generator.generate(1000)
val preprocessed = new RankIndex(raw_data)
val ksp = KSP(M = 50, alpha = 0.5, parallelize = 1)

val contrast_random_benchmark = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 100000)
// then we compare the random ref dim and iterate ref dim
val contrast_random = ksp.contrast_random_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)
val contrast_iterate = ksp.contrast_iterate_ref_dim(preprocessed, raw_data(0).indices.toSet, MC_num = 10000)


// conclusion, iterating the reference dims or randomly selects the reference dimension
// all yields the same contrast, unbiased.