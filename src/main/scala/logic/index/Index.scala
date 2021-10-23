package logic.index

import scala.annotation.tailrec

trait Index {
  val values: Array[Array[Double]]

  type T
  val index: Array[Array[T]] = createIndex(values.transpose) // IMPORTANT: The transpose, makes the input column-oriented

  /**
    *
    * @param data a data set (column-oriented!)
    * @return An index, which is also column-oriented
    */
  protected def createIndex(data: Array[Array[Double]]): Array[Array[T]]

  def apply(n: Int) = index(n) // access the columns of the index
  
  // return the indices of the dimensions
  def indices = index.indices

  // return the number of dimensions
  def num_dims = index.length

  // return the number of observations
  def num_obs = index(0).length

  // return if the index(original data) is empty
  def isEmpty: Boolean = index.length == 0

  /**
    * Returns an array of booleans specifying the two disjoint sets of the observations for further
    * statistical tests. True corresponds to the set 1 and false corresponds to the set 2.
    * The reference dimension is contained in the dimensions.
    * The array is obtained by performing subspace slicing on the subspace
    * of dimensions dims/ref_dim with reference dimension ref_dim.
    * @param dims The set of dimensions of the subspaces
    * @param ref_dim The dimension that is considered as reference
    * @param sliceSize The size of the slice for each dimensions, determined by alpha
    * @return Returns an array of booleans. It specifies the two disjoint sets of the observations
    * for further statistical tests. True corresponds to the set 1 and false corresponds to the set 2.
    */
  def slice_with_ref_dim(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean]


  /**
   * Returns an array of booleans specifying the two disjoint sets of the observations for further
   * statistical tests. True corresponds to the set 1 and false corresponds to the set 2.
   * The array is obtained by performing subspace slicing on the subspace of dimensions dims.
   * The reference dimension is not contained in the dimensions
   * @param dims The set of dimensions of the subspaces
   * @param sliceSize The size of the slice for each dimensions, determined by alpha
   * @return Returns an array of booleans. It specifies the two disjoint sets of the observations
   * for further statistical tests. True corresponds to the set 1 and false corresponds to the set 2.
   */
  def slice_without_ref_dim(dims: Set[Int], sliceSize: Int): Array[Boolean]
}
