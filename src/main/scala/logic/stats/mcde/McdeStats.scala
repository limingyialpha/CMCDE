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
package logic.stats.mcde

import scala.collection.parallel.ForkJoinTaskSupport
import logic.index.Index

/**
  * Created by fouchee on 07.07.17.
  * alpha Expected share of instances in slice (independent dimensions).
  */
trait McdeStats{

  type PreprocessedData <: Index
  val id: String
  val alpha: Double
  val M: Int
  var parallelize: Int

  require(alpha > 0 & alpha < 1, "alpha should be greater than 0 and lower than 1")
  require(M > 0, "M should be greater than 0")

  /**
    * Statistical test computation
    *
    * @param reference      The vector of the reference dimension as an array of 2-Tuple. First element is the index, the second is the rank
    * @param indexSelection An array of Boolean that contains the information if a given index is in the slice
    */
  def twoSample(index: PreprocessedData, reference: Int, indexSelection: Array[Boolean]): Double

  def influence(m: PreprocessedData, from_dims: Set[Int], to_dims: Set[Int], MC_num:Int = M): Double = {
    val sliceSize = (math.pow(alpha, 1.0 / (from_dims.size)) * m.num_obs).ceil.toInt
    //println(s"slice size is: ${sliceSize}")

    val num_to_dims =to_dims.size
    //println(s"number of to dimensions is:${to_dims.size}")
    // for example 50 MC iterations with 8 dimensions 50/8*8 = 48
    val upper_boarder: Int = MC_num/num_to_dims * num_to_dims
    //println(s"upper boarder is ${upper_boarder}")
    val to_dims_vec = to_dims.toVector

    val result = if (parallelize == 0) {
      (1 to MC_num).map(i => {
        val referenceDim = if (i <= upper_boarder) to_dims_vec((i-1) % num_to_dims) else to_dims_vec(scala.util.Random.nextInt(num_to_dims))
        val result = twoSample(m, referenceDim, m.slice_without_ref_dim(from_dims, sliceSize))
        //println(s"now is mc iteration num: ${i}, reference dimension is: ${referenceDim}, and the test p value is ${result}")
        result
      }).sum / MC_num
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = if (i <= upper_boarder) to_dims_vec((i-1) % num_to_dims) else to_dims_vec(scala.util.Random.nextInt(num_to_dims))
        val result = twoSample(m, referenceDim, m.slice_without_ref_dim(from_dims, sliceSize))
        //println(s"now is mc iteration num: ${i}, reference dimension is: ${referenceDim}, and the test p value is ${result}")
        result
      }).sum / MC_num
    }
    result
  }


  /**
    * Compute the contrast of a subspace
    *
    * @param m          The indexes from the original data ordered by the rank of the points
    * @param dimensions The dimensions in the subspace, each value should be smaller than the number of arrays in m
    * @return The contrast of the subspace (value between 0 and 1)
    */
  def contrast_random_ref_dim(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Double = {
    // Sanity check
    // require(dimensions.forall(x => x>=0 & x < m.length), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")

    val result = if (parallelize == 0) {
      (1 to MC_num).map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
      }).sum / MC_num
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
        twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
      }).sum / MC_num
    }
    result
  }


// dimensions starts from 0
  def contrast_iterate_ref_dim(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Double = {
    // Sanity check
    //require(dimensions.forall(x => x>=0 & x < m.length), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")
    val num_dims = dimensions.size
    // for example 50 MC iterations with 8 dimensions 50/8*8 = 48
    val upper_boarder: Int = MC_num/num_dims * num_dims

    val dims_vec = dimensions.toVector

    val result = if (parallelize == 0) {
      (1 to MC_num).map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i-1)%num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
      }).sum / MC_num
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i-1)%num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
      }).sum / MC_num
    }
    result
  }

  def canonical_contrast_pair(m: PreprocessedData, first_set_dims: Set[Int], second_set_dims:Set[Int], MC_num:Int = M): (Double,Double)={
    val influence_1_to_2 = influence(m,from_dims = first_set_dims,to_dims = second_set_dims, MC_num = MC_num)
    val influence_2_to_1 = influence(m,from_dims = second_set_dims,to_dims = first_set_dims, MC_num = MC_num)
    (influence_1_to_2,influence_2_to_1)
  }

  def canonical_contrast_unweighted(m: PreprocessedData, first_set_dims: Set[Int], second_set_dims:Set[Int], MC_num:Int = M): Double ={
    val (influence_1_to_2,influence_2_to_1) = canonical_contrast_pair(m, first_set_dims, second_set_dims, MC_num)
    (influence_1_to_2 + influence_2_to_1)/2
  }

  def canonical_contrast_weighted_by_from_dims(m: PreprocessedData, first_set_dims: Set[Int], second_set_dims:Set[Int], MC_num:Int = M): Double= {
    val (influence_1_to_2, influence_2_to_1) = canonical_contrast_pair(m, first_set_dims, second_set_dims, MC_num)
    (influence_1_to_2 * first_set_dims.size + influence_2_to_1 * second_set_dims.size) / (first_set_dims.size + second_set_dims.size)
  }

  def canonical_contrast_weighted_by_to_dims(m: PreprocessedData, first_set_dims: Set[Int], second_set_dims:Set[Int], MC_num:Int = M): Double= {
    val (influence_1_to_2, influence_2_to_1) = canonical_contrast_pair(m, first_set_dims, second_set_dims, MC_num)
    (influence_1_to_2 * second_set_dims.size + influence_2_to_1 * first_set_dims.size) / (first_set_dims.size + second_set_dims.size)
  }
}
