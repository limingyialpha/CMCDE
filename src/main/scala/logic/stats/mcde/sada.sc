import io.github.edouardfouche.generators.{Independent, Linear}
import logic.index.RankIndex
import logic.stats.mcde.KSP

val dim = 4
val i = Independent(dim, 0, "gaussian", 0).generate(1000)
val l = Linear(dim, 0.6, "gaussian", 0).generate(1000)
val m = new RankIndex(l)
val dims_set = (0 until dim).toSet
val ksp = KSP(parallelize = 1)
//ksp.contrast(m, dims_set, 50)(estimator = "R")
//ksp.contrast(m, dims_set, 50)(estimator = "ItR")
ksp.contrast(m, dims_set, 50)(estimator = "ItGR")
ksp.contrast(m, dims_set, 50)(estimator = "ItGI")
//ksp.contrast(m, dims_set, 50)(estimator = "ItGIBEV")