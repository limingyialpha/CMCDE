package logic.index

/**
 * Implementation of the Rankindex in MCDE and its slice algorithms, for numerical values.
 */
class RankIndex(val values: Array[Array[Double]]) extends Index {
  type T = Int

  /**
   * A very simple index structure with only the ranks
   */
  def createIndex(input: Array[Array[Double]]): Array[Array[T]] = {
    input.map(_.zipWithIndex.sortBy(_._1).map(x => x._2))
  }

  /**
   * The slice can only be in the center, it does not cross the boarder
   */
  def slice_with_ref_dim_center(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims.filter(_ != ref_dim)} {
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

  /**
   * The slice can start anywhere, and cross the boarder.
   * When the number of dimensions are high, slice size is bigger, and the slice almost always crosses the boarder.
   */
  def slice_with_ref_dim_uniform(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims.filter(_ != ref_dim)} {
      val counter_slice_size = m(0).length - sliceSize
      val counter_sliceStart = scala.util.Random.nextInt((m(0).length).max(1))
      val to_set_false = (counter_sliceStart until counter_sliceStart + counter_slice_size - 1).map(x => x % m(0).length)
      for {x <- to_set_false} {
        logicalArray(m(dim)(x)) = false
      }
    }
    logicalArray
  }

  /**
   * A compromise between center slice and uniform slice. 50% of the time the slice is in the middles.
   * 50% of time it crosses the boarder
   */
  def slice_with_ref_dim_semi_uniform(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims.filter(_ != ref_dim)} {
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


  /**
   * The slice can only be in the center, it does not cross the boarder
   */
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

  /**
   * The slice can start anywhere, and cross the boarder.
   * When the number of dimensions are high, slice size is bigger, and the slice almost always crosses the boarder.
   */
  def slice_without_ref_dim_uniform(dims: Set[Int], sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dims} {
      val counter_slice_size = m(0).length - sliceSize
      val counter_sliceStart = scala.util.Random.nextInt((m(0).length).max(1))
      val to_set_false = (counter_sliceStart until counter_sliceStart + counter_slice_size - 1)
        .map(x => x % m(0).length)
      for {x <- to_set_false} {
        logicalArray(m(dim)(x)) = false
      }
    }
    logicalArray
  }


  /**
   * A compromise between center slice and uniform slice. 50% of the time the slice is in the middles.
   * 50% of time it crosses the boarder
   */
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
