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
import logic.data.Utility.{cumulative_average, cumulative_sum}
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
   * random
   * @param m
   * @param dimensions
   * @param MC_num
   * @return
   */

  def contrast_random_ref_dim_vec(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
    // Sanity check
    // require(dimensions.forall(x => x>=0 & x < m.length), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")

    val result = {
      if (parallelize == 0) {
        (1 to MC_num).map(i => {
          val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
          twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
        }).toVector
      }
      else {
        val iterations = (1 to MC_num).par
        if (parallelize > 1) {
          iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
        }
        iterations.map(i => {
          val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
          (i,twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize)))
        }).toVector.sortBy(_._1).map(x => x._2)
      }
    }
    result
  }


  def contrast_random_ref_dim(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Double = {
    // Sanity check
    // require(dimensions.forall(x => x>=0 & x < m.length), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")

    val result = {
      if (parallelize == 0) {
        (1 to MC_num).map(i => {
          val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
          twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
        }).sum/MC_num
      }
      else {
        val iterations = (1 to MC_num).par
        if (parallelize > 1) {
          iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
        }
        iterations.map(i => {
          val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
          twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
        }).sum/MC_num
      }
    }
    result
  }

  def contrast_random_ref_dim_cumulative_average_vec(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
    cumulative_average(contrast_random_ref_dim_vec(m, dimensions, MC_num))
  }


  /**
   * iterate tail MC
   * @param m
   * @param dimensions
   * @param MC_num
   * @return
   */

  def contrast_iterate_ref_dim_tail_MC_vec(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
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
      }).toVector
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i-1)%num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        (i,twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize)))
      }).toVector.sortBy(_._1).map(x => x._2)
    }
    result
  }

 //dimensions starts from 0
  def contrast_iterate_ref_dim_tail_MC(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Double = {
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
      }).sum/MC_num
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i-1)%num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
      }).sum/MC_num
    }
    result
  }



  def contrast_iterate_ref_dim_tail_MC_cumulative_average_vec(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
    cumulative_average(contrast_iterate_ref_dim_tail_MC_vec(m,dimensions, MC_num))
  }

  // for example, 7 iterations left, 100 dimensions, then we have(15,15,14,14,14,14,14),
  // 7 nearly equal size bins
  def get_tail_bin_sizes(num_dims: Int, tail_MC_num: Int): Vector[Int]={
    require((num_dims > tail_MC_num), "The number of dimensions should greater than the tail MC_number")
    (0 until tail_MC_num).map(x => {
      val overflow = if (x < num_dims % tail_MC_num) 1 else 0
      (overflow + num_dims/tail_MC_num)
    }).toVector
  }

  /**
   * iterate tail random ref dim grouping
    */
  def contrast_iterate_ref_dim_tail_ref_dim_grouping_random(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Double = {
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1

    val dims_vec = dimensions.toVector

    val num_dims = dimensions.size

    // for example 50 MC iterations with 8 dimensions: 50/8 = 6 head iterations
    val head_great_iteration_num: Int = MC_num/num_dims
    // 6*8 = 48 small MC iterations
    val head_MC_number: Int = head_great_iteration_num * num_dims
    // tail MC number is then 50 - 48 = 2
    val tail_MC_number: Int = MC_num - head_MC_number

    val head_contrast = if (head_MC_number >= 1) {
      if (parallelize == 0) {
        (1 to head_MC_number).map(i => {
          val referenceDim = dims_vec((i-1)%num_dims)
          twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
        }).sum/head_MC_number
      } else {
        val iterations = (1 to MC_num).par
        if (parallelize > 1) {
          iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
        }
        iterations.map(i => {
          val referenceDim = dims_vec((i-1)%num_dims)
          twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize))
        }).sum/head_MC_number
      }
    } else 0

    // for example, 7 iterations left, 100 dimensions, then we have
    // (15,15,14,14,14,14,14), 7 nearly equal size bins
    val tail_bins_sizes = get_tail_bin_sizes(num_dims, tail_MC_number)
    //(15, 30, 44, 58, 72, 86, 100)
    val tail_bins_start_index = cumulative_sum(tail_bins_sizes)

    val tail_contrast= (0 until tail_MC_number).map(x=>{
      val current_bin_size = tail_bins_sizes(x)
      // crucial
      val dims_in_the_bin = dims_vec.slice(tail_bins_start_index(x)-current_bin_size,tail_bins_start_index(x))
      val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
      twoSample(m, ref_dim, m.slice_with_ref_dim(dimensions, ref_dim, sliceSize)) * (current_bin_size/ num_dims)
    }).sum * tail_MC_number/MC_num

    val contrast = (head_contrast * head_MC_number + tail_contrast * tail_MC_number)/ MC_num
    contrast
  }


  /**
   * iterate & uniform
   * @param m
   * @param dimensions
   * @param MC_num
   * @return
   */

  def contrast_iterate_uniform_vec(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
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
        twoSample(m, referenceDim, m.slice_with_ref_dim_uniform(dimensions, referenceDim, sliceSize))
      }).toVector
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i-1)%num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        (i,twoSample(m, referenceDim, m.slice_with_ref_dim_uniform(dimensions, referenceDim, sliceSize)))
      }).toVector.sortBy(_._1).map(x => x._2)
    }
    result
  }



  // dimensions starts from 0
  def contrast_iterate_uniform(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Double = {
    contrast_iterate_uniform_vec(m, dimensions, MC_num).sum/MC_num
  }

  def contrast_iterate_uniform_cumulative_average_vec(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
    cumulative_average(contrast_iterate_uniform_vec(m,dimensions, MC_num))
  }


  /**
   * iterate & edouard uniform
   * @param m
   * @param dimensions
   * @param MC_num
   * @return
   */
  def contrast_iterate_uniform_vec_edouard(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
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
        twoSample(m, referenceDim, m.slice_with_ref_dim_uniform_edouard(dimensions, referenceDim, sliceSize))
      }).toVector
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i-1)%num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        (i,twoSample(m, referenceDim, m.slice_with_ref_dim_uniform_edouard(dimensions, referenceDim, sliceSize)))
      }).toVector.sortBy(_._1).map(x => x._2)
    }
    result
  }

  // dimensions starts from 0
  def contrast_iterate_uniform_edouard(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Double = {
    contrast_iterate_uniform_vec_edouard(m, dimensions, MC_num).sum/MC_num
  }

  def contrast_iterate_uniform_cumulative_average_vec_edouard(m: PreprocessedData, dimensions: Set[Int], MC_num:Int = M): Vector[Double] = {
    cumulative_average(contrast_iterate_uniform_vec_edouard(m,dimensions, MC_num))
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
