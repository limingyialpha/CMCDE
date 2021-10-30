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

  def slice_with_ref_dim(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean]

  def slice_without_ref_dim(dims: Set[Int], sliceSize: Int): Array[Boolean]

  def slice_with_ref_dim_uniform(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean]

  def slice_without_ref_dim_uniform(dims: Set[Int], sliceSize: Int): Array[Boolean]

  def slice_with_ref_dim_uniform_edouard(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean]

  //def slice_without_ref_dim_uniform_edouard(dims: Set[Int], sliceSize: Int): Array[Boolean]
}
