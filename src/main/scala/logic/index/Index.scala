package logic.index

/**
 * Implementation of Index in Monte Carlo Dependency (MCDE)
 */
trait Index {
  val values: Array[Array[Double]]
  type T
  val index: Array[Array[T]] = createIndex(values.transpose) // IMPORTANT: The transpose, makes the input column-oriented

  /**
   * We look into 3 slice techniques.
   * "c" is the center slice, the one in MCDE
   * "su" is the semi-uniform slice, see implementation in Rankindex
   * "u" is the uniform slice, see implementation in Rankindex
   */
  val slice_techniques = Set("c", "su", "u")

  /**
   * Create index from data
   *
   * @param data a data set (column-oriented!)
   * @return An index, which is also column-oriented
   */
  protected def createIndex(data: Array[Array[Double]]): Array[Array[T]]

  // access the columns of the index
  def apply(n: Int): Array[T] = index(n)

  // return the indices of the dimensions
  def indices: Range = index.indices

  // return the number of dimensions
  def num_dims: Int = index.length

  // return the number of observations
  def num_obs: Int = index(0).length

  // return if the index(original data) is empty
  def isEmpty: Boolean = index.length == 0

  /**
   * Slice with reference dimension contained in the set of dimensions
   */
  def slice_with_ref_dim(dims: Set[Int], ref_dim: Int, sliceSize: Int)(slice_technique: String = "c"): Array[Boolean] = {
    slice_technique match {
      case "c" => slice_with_ref_dim_center(dims, ref_dim, sliceSize)
      case "su" => slice_with_ref_dim_semi_uniform(dims, ref_dim, sliceSize)
      case "u" => slice_with_ref_dim_uniform(dims, ref_dim, sliceSize)
    }
  }

  /**
   * Slice the set of dimensions
   */
  def slice_without_ref_dim(dims: Set[Int], sliceSize: Int)(slice_technique: String = "c"): Array[Boolean] = {
    slice_technique match {
      case "c" => slice_without_ref_dim_center(dims, sliceSize)
      case "su" => slice_without_ref_dim_semi_uniform(dims, sliceSize)
      case "u" => slice_without_ref_dim_uniform(dims, sliceSize)
    }
  }

  def slice_with_ref_dim_center(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean]

  def slice_without_ref_dim_center(dims: Set[Int], sliceSize: Int): Array[Boolean]

  def slice_with_ref_dim_semi_uniform(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean]

  def slice_without_ref_dim_semi_uniform(dims: Set[Int], sliceSize: Int): Array[Boolean]

  def slice_with_ref_dim_uniform(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean]

  def slice_without_ref_dim_uniform(dims: Set[Int], sliceSize: Int): Array[Boolean]
}
