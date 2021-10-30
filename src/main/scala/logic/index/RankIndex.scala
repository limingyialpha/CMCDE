/*
 * Copyright (C) 2018 Edouard Fouché
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package logic.index

import scala.collection.parallel.ForkJoinTaskSupport

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

  def slice_with_ref_dim(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
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

  def slice_with_ref_dim_uniform_edouard(dims: Set[Int], ref_dim: Int, sliceSize: Int): Array[Boolean] = {
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



  def slice_without_ref_dim(dimensions: Set[Int], sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dimensions} {
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

  def slice_without_ref_dim_uniform(dimensions: Set[Int], sliceSize: Int): Array[Boolean] = {
    val m = this.index
    val logicalArray = Array.fill[Boolean](m(0).length)(true)
    for {dim <- dimensions} {
      val counter_slice_size = m(0).length-sliceSize
      val counter_sliceStart = scala.util.Random.nextInt((m(0).length).max(1))
      val to_set_false = (counter_sliceStart until counter_sliceStart + counter_slice_size-1)
        .map(x => x % m(0).length)
      for {x <- to_set_false} {logicalArray(m(dim)(x)) = false}
    }
    logicalArray
  }
}
