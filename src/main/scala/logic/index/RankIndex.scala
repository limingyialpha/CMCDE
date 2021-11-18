package logic.index

/**
  * A very simple index structure will only the ranks (convenient for HiCS for example)
  * @param values
  */
class RankIndex(val values: Array[Array[Double]]) extends Index {
  type T = Int

  /**
   * Return the rank logic.index structure (as in HiCS).
   *
   * Note that the numbers might be different in the case of ties, in comparison with other implementations.
   *
   * @param input A 2-D Array of Double (data set, column-oriented).
   * @return A 2-D Array of Int, where the element is the original logic.index in the unsorted data set
   */
  def createIndex(input: Array[Array[Double]]): Array[Array[T]] = {
    input.map(_.zipWithIndex.sortBy(_._1).map(x => x._2))
  }

  def slice_with_ref_dim_center(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims.filter(_ != ref_dim)} {
      val sliceStart = scala.util.Random.nextInt((m(0).length - sliceSize).max(1))
      for {x <- 0 until sliceStart} {logicalArray(m(dim)(x)) = false}
      for {x <- sliceStart + sliceSize until m(0).length} {logicalArray(m(dim)(x)) = false}
    }
    logicalArray
  }

  // each time, a slice of size sliceSize of trues will be chosen
  def slice_with_ref_dim_uniform(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims.filter(_ != ref_dim)} {
      val counter_slice_size = m(0).length-sliceSize
      val counter_sliceStart = scala.util.Random.nextInt((m(0).length).max(1))
      val to_set_false = (counter_sliceStart until counter_sliceStart + counter_slice_size-1).map(x => x % m(0).length)
      for {x <- to_set_false} {logicalArray(m(dim)(x)) = false}
    }
    logicalArray
  }

  def slice_with_ref_dim_semi_uniform(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims.filter(_ != ref_dim)} {
      val flag = scala.util.Random.nextInt(2)
      if(flag == 1) {
        val sliceStart = scala.util.Random.nextInt((m(0).length - sliceSize).max(1))
        for {x <- 0 until sliceStart} {logicalArray(m(dim)(x)) = false}
        for {x <- sliceStart + sliceSize until m(0).length} {logicalArray(m(dim)(x)) = false}
      } else {
        val counterSliceSize = m(0).length-sliceSize // This slice has to be the opposite size (usually smaller)
        // because we exclude everything that is in it, and we need to exclude the same number of items in both cases.
        val sliceStart = scala.util.Random.nextInt((m(0).length - counterSliceSize).max(1))
        for {x <- sliceStart until sliceStart + counterSliceSize} {logicalArray(m(dim)(x)) = false}
      }
    }
    logicalArray
  }



  def slice_without_ref_dim_center(dims: Set[Int], sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims} {
      val sliceStart = scala.util.Random.nextInt((m(0).length - sliceSize).max(1))
      for {x <- 0 until sliceStart} {
        logicalArray(m(dim)(x)) = false
      }
      for {x <- sliceStart + sliceSize until m(0).length} {
        logicalArray(m(dim)(x)) = false
      }
    }
    logicalArray
  }

  def slice_without_ref_dim_uniform(dims: Set[Int], sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims} {
      val counter_slice_size = m(0).length-sliceSize
      val counter_sliceStart = scala.util.Random.nextInt((m(0).length).max(1))
      val to_set_false = (counter_sliceStart until counter_sliceStart + counter_slice_size-1)
        .map(x => x % m(0).length)
      for {x <- to_set_false} {logicalArray(m(dim)(x)) = false}
    }
    logicalArray
  }

  def slice_without_ref_dim_semi_uniform(dims: Set[Int], sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims} {
      val flag = scala.util.Random.nextInt(2)
      if (flag == 1) {
        val sliceStart = scala.util.Random.nextInt((m(0).length - sliceSize).max(1))
        for {x <- 0 until sliceStart} {
          logicalArray(m(dim)(x)) = false
        }
        for {x <- sliceStart + sliceSize until m(0).length} {
          logicalArray(m(dim)(x)) = false
        }
      } else {
        val counterSliceSize = m(0).length - sliceSize // This slice has to be the opposite size (usually smaller)
        // because we exclude everything that is in it, and we need to exclude the same number of items in both cases.
        val sliceStart = scala.util.Random.nextInt((m(0).length - counterSliceSize).max(1))
        for {x <- sliceStart until sliceStart + counterSliceSize} {
          logicalArray(m(dim)(x)) = false
        }
      }
    }
    logicalArray
  }
}
