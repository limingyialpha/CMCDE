package logic.gmcde

import logic.index.RankIndex
import logic.stats.mcde.KSP

/**
 * A wrapper for GMCDE. Hides details like Rankindex and ksp stats... from user
 * Uses the Generalized contrast and contrast internally
 */
case class GMCDE(parallelize: Int = 0, MC_num: Int = 50){
  val id = "GMCDE"
  val alpha = 0.5
  val slice_technique = "c"

  def contrast(data: Array[Array[Double]], dims: Set[Int])(estimator: String, slice_technique: String): Double = {
    val preprocessed = new RankIndex(data)
    val ksp = KSP(MC_num,alpha,parallelize)
    ksp.contrast(preprocessed, dims, MC_num)(estimator, slice_technique)
  }

  def canonical_contrast(data: Array[Array[Double]], dims_x: Set[Int], dims_y: Set[Int]): Double = {
    val preprocessed = new RankIndex(data)
    val ksp = KSP(MC_num,alpha,parallelize)
    ksp.generalized_contrast(preprocessed, Set(dims_x, dims_y))(slice_technique)
  }

  def generalized_contrast(data: Array[Array[Double]], groups_of_dims: Set[Set[Int]]): Double = {
    val preprocessed = new RankIndex(data)
    val ksp = KSP(MC_num,alpha,parallelize)
    ksp.generalized_contrast(preprocessed, groups_of_dims)(slice_technique)
  }
}
