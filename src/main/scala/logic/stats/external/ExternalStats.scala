package logic.stats.external

trait ExternalStats {
  var id: String
  var parallelize: Int

  def canonical_contrast(data: Array[Array[Double]], dims_x: Set[Int], dims_y: Set[Int]): Double

  def canonical_test_independent(data: Array[Array[Double]], dims_x: Set[Int], dims_y: Set[Int]): Boolean

  def canonical_contrast_test_independent(data: Array[Array[Double]], dims_x: Set[Int], dims_y: Set[Int]): (Double, Boolean)
}
