package logic.stats.mcde

import scala.collection.parallel.ForkJoinTaskSupport
import logic.index.Index
import logic.data.Utility.cumulative_sum
// breeze.stats.variance computes the empirical variance
import breeze.stats.variance

import scala.language.postfixOps

trait McdeStats {
  type PreprocessedData <: Index
  val id: String
  // alpha is the expected share of instances in slice.
  val alpha: Double
  // Iteration number for contrast / generalized contrast approximation.
  val M: Int
  // level of parallelism
  var parallelize: Int
  // these are the possible dependency estimators for contrast approximation
  val contrast_dependency_estimators = Set("R", "ItR", "ItGR", "ItGI", "ItGIBEV")

  /**
   * Statistical test computation
   *
   * @param reference      The vector of the reference dimension as an array of 2-Tuple. First element is the index, the second is the rank
   * @param indexSelection An array of Boolean that contains the information if a given index is in the slice
   */
  def twoSample(index: PreprocessedData, reference: Int, indexSelection: Array[Boolean]): Double


  /**
   * the factory method for contrast computation by different dependency estimators
   * Names:
   * "R" => Randomly choosing reference dimensions
   * "ItR" => head: Iterating reference dimensions, Tail: Randomly choosing reference dimensions
   * "ItGR" => head: Iterating reference dimensions, Tail: Grouping dimensions Randomly
   * "ItGI" => head: Iterating reference dimensions, Tail: Grouping dimensions by tracked Influence
   * "ItGIBEV" => head: Iterating reference dimensions, Tail: Grouping dimensions by tracked Influence, Balance head and tail with Empirical Variance
   * Implementations and details below and in GMCDE paper.
   */
  def contrast(m: PreprocessedData, dimensions: Set[Int], MC_num: Int = M)(estimator: String = "R", slice_technique: String = "c"): Double = {
    estimator match {
      case "R" => contrast_random_ref_dim(m, dimensions, MC_num)(slice_technique)
      case "ItR" => contrast_iterate_ref_dim_tail_random(m, dimensions, MC_num)(slice_technique)
      case "ItGR" => contrast_iterate_ref_dim_tail_group_ref_dim_randomly(m, dimensions, MC_num)(slice_technique)
      case "ItGI" => contrast_iterate_ref_dim_tail_group_ref_dim_by_influence(m, dimensions, MC_num)(slice_technique)
      case "ItGIBEV" => contrast_iterate_ref_dim_tail_group_ref_dim_by_influence_balance_by_empirical_variance(m, dimensions, MC_num)(slice_technique)
    }
  }

  /**
   * Original dependency estimator in MCDE where reference dimensions are randomly chosen
   * the true Monte Carlo
   */
  def contrast_random_ref_dim(m: PreprocessedData, dimensions: Set[Int], MC_num: Int = M)(slice_technique: String = "c"): Double = {
    // Sanity check
    // require(dimensions.forall(x => x>=0 & x < m.length), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")

    val result = {
      if (parallelize == 0) {
        (1 to MC_num).map(_ => {
          val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
          twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize)(slice_technique))
        }).sum / MC_num
      }
      else {
        val iterations = (1 to MC_num).par
        if (parallelize > 1) {
          iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
        }
        iterations.map(_ => {
          val referenceDim = dimensions.toVector(scala.util.Random.nextInt(dimensions.size))
          twoSample(m, referenceDim, m.slice_with_ref_dim(dimensions, referenceDim, sliceSize)(slice_technique))
        }).sum / MC_num
      }
    }
    result
  }

  /**
   * reference dimensions are iterated in the head. In the Tail, we choose reference dimensions randomly.
   * We get two estimators from both head and tail, and we balance them with their iteration numbers.
   * 30 dimensions 70 iterations => head = 30 * 2 = 60 iterations, tail = 70 % 30 = 10 iterations
   * or 30 dimensions 20 iterations => head = 0 iteration, tail = 20 % 30 = 20 iterations
   */
  def contrast_iterate_ref_dim_tail_random(m: PreprocessedData, dimensions: Set[Int], MC_num: Int = M)(slice_technique: String = "c"): Double = {
    // Sanity check
    //require(dimensions.forall(x => x>=0 & x < m.length), "The dimensions for deviation need to be greater or equal to 0 and lower than the total number of dimensions")
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1
    //println(s"dimensions $dimensions, sliceSize: ${sliceSize}")
    val num_dims = dimensions.size
    // for example 50 MC iterations with 8 dimensions 50/8*8 = 48
    val upper_boarder: Int = MC_num / num_dims * num_dims

    val dims_vec = dimensions.toVector

    val dims_set = dimensions

    val result = if (parallelize == 0) {
      (1 to MC_num).map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i - 1) % num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
      }).sum / MC_num
    } else {
      val iterations = (1 to MC_num).par
      if (parallelize > 1) {
        iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
      }
      iterations.map(i => {
        val referenceDim = if (i <= upper_boarder) dims_vec((i - 1) % num_dims) else dims_vec(scala.util.Random.nextInt(num_dims))
        twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
      }).sum / MC_num
    }
    result
  }


  // for example, 7 iterations left, 100 dimensions, then we have(15,15,14,14,14,14,14),
  // 7 nearly equal size bins
  def get_tail_bin_sizes(num_dims: Int, tail_MC_num: Int): Vector[Int] = {
    require(num_dims > tail_MC_num, "The number of dimensions should greater than the tail MC_number")
    (0 until tail_MC_num).map(x => {
      val overflow = if (x < num_dims % tail_MC_num) 1 else 0
      overflow + num_dims / tail_MC_num
    }).toVector
  }

  /**
   * reference dimensions are iterated in the head. In the Tail, we group dimensions randomly in nearly equal-sized bins
   * with number of bins = number of tail iterations. Then we choose reference dimensions from the bins.
   * We get two estimators from both head and tail, and we balance them with their iteration numbers.
   * 30 dimensions 70 iterations => head = 30 * 2 = 60 iterations, tail = 70 % 30 = 10 iterations
   * or 30 dimensions 20 iterations => head = 0 iteration, tail = 20 % 30 = 20 iterations
   */
  def contrast_iterate_ref_dim_tail_group_ref_dim_randomly(m: PreprocessedData, dimensions: Set[Int], MC_num: Int = M)(slice_technique: String = "c"): Double = {
    val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1

    val dims_vec = dimensions.toVector

    val dims_set = dimensions

    val num_dims = dimensions.size

    // for example 50 MC iterations with 8 dimensions: 50/8 = 6 head iterations
    val head_great_iteration_num: Int = MC_num / num_dims
    // 6*8 = 48 small MC iterations
    val head_MC_number: Int = head_great_iteration_num * num_dims
    // tail MC number is then 50 - 48 = 2
    val tail_MC_number: Int = MC_num - head_MC_number

    val head_contrast = if (head_MC_number >= 1) {
      if (parallelize == 0) {
        (1 to head_MC_number).map(i => {
          val referenceDim = dims_vec((i - 1) % num_dims)
          twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
        }).sum / head_MC_number
      } else {
        val iterations = (1 to head_MC_number).par
        if (parallelize > 1) {
          iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
        }
        iterations.map(i => {
          val referenceDim = dims_vec((i - 1) % num_dims)
          twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
        }).sum / head_MC_number
      }
    } else 0

    val tail_contrast = if (tail_MC_number >= 1) {

      // for example, 7 iterations left, 100 dimensions, then we have
      // (15,15,14,14,14,14,14), 7 nearly equal size bins
      val tail_bins_sizes = get_tail_bin_sizes(num_dims, tail_MC_number)

      //(15, 30, 44, 58, 72, 86, 100)
      val tail_bins_start_index = cumulative_sum(tail_bins_sizes)

      if (parallelize == 0) {
        (0 until tail_MC_number).map(x => {
          val current_bin_size = tail_bins_sizes(x)
          // crucial
          val dims_in_the_bin = dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
          val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
          // do not forget current_bin_size.toDouble!!!
          twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, sliceSize)(slice_technique)) * (current_bin_size.toDouble / num_dims)
        }).sum
      } else {
        val iterations = (0 until tail_MC_number).par
        if (parallelize > 1) {
          iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
        }
        iterations.map(x => {
          val current_bin_size = tail_bins_sizes(x)
          // crucial
          val dims_in_the_bin = dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
          val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
          twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, sliceSize)(slice_technique)) * (current_bin_size.toDouble / num_dims)
        }).sum
      }
    } else 0

    val contrast = (head_contrast * head_MC_number + tail_contrast * tail_MC_number) / MC_num
    contrast
  }


  /**
   * reference dimensions are iterated in the head and the reference dimension-dependent influences are tracked.
   * In the Tail, we first sort dimensions by their influences in the head.
   * Then we group dimensions in nearly equal-sized bins with number of bins = number of tail iterations.
   * Then we choose reference dimensions from the bins.
   * We get two estimators from both head and tail, and we balance them with their iteration numbers.
   * 30 dimensions 70 iterations => head = 30 * 2 = 60 iterations, tail = 70 % 30 = 10 iterations
   * or 30 dimensions 20 iterations => head = 0 iteration, tail = 20 % 30 = 20 iterations
   */
  def contrast_iterate_ref_dim_tail_group_ref_dim_by_influence(m: PreprocessedData, dimensions: Set[Int], MC_num: Int = M)(slice_technique: String = "c"): Double = {
    if (MC_num == 0) -1 else {
      val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1

      val dims_vec = dimensions.toVector

      val dims_set = dimensions

      val num_dims = dimensions.size

      // for example 50 MC iterations with 8 dimensions: 50/8 = 6 head iterations
      val head_great_iteration_num: Int = MC_num / num_dims
      // 6*8 = 48 small MC iterations
      val head_MC_number: Int = head_great_iteration_num * num_dims
      // tail MC number is then 50 - 48 = 2
      val tail_MC_number: Int = MC_num - head_MC_number

      val head_influence_vec: Vector[Double] = if (head_MC_number >= 1) {
        if (parallelize == 0) {
          (1 to head_MC_number).map(i => {
            val referenceDim = dims_vec((i - 1) % num_dims)
            twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
          }).toVector
        } else {
          val iterations = (1 to head_MC_number).par
          if (parallelize > 1) {
            iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
          }
          iterations.map(i => {
            val referenceDim = dims_vec((i - 1) % num_dims)
            twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
          }).toVector
        }
      } else Vector(0.0) // a dumb variable that will never be used

      val head_contrast = if (head_MC_number >= 1) head_influence_vec.sum / head_MC_number else 0

      val tail_contrast = if (tail_MC_number >= 1) {

        // if head_number = 0 => we have not got information about the influences from each dimension
        // we simply use dims_vec else, we can analyze the influence vector from the previous iterations
        // and sort the dim vec by their sum of influences
        val sorted_dims_vec = if (head_MC_number == 0) dims_vec else {
          val zipped = head_influence_vec.zipWithIndex
          (0 until num_dims).map(i => {
            val selected_vec = zipped.filter(x => x._2 % num_dims == i).map(x => x._1)
            selected_vec.sum
          }).zip(dims_vec).sortBy(x => x._1).map(x => x._2).toVector
        }

        // for example, 7 iterations left, 100 dimensions, then we have
        // (15,15,14,14,14,14,14), 7 nearly equal size bins
        val tail_bins_sizes = get_tail_bin_sizes(num_dims, tail_MC_number)

        //(15, 30, 44, 58, 72, 86, 100)
        val tail_bins_start_index = cumulative_sum(tail_bins_sizes)

        if (parallelize == 0) {
          (0 until tail_MC_number).map(x => {
            val current_bin_size = tail_bins_sizes(x)
            // crucial
            val dims_in_the_bin = sorted_dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
            val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
            // do not forget current_bin_size.toDouble!!!
            twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, sliceSize)(slice_technique)) * (current_bin_size.toDouble / num_dims)
          }).sum
        } else {
          val iterations = (0 until tail_MC_number).par
          if (parallelize > 1) {
            iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
          }
          iterations.map(x => {
            val current_bin_size = tail_bins_sizes(x)
            // crucial
            val dims_in_the_bin = sorted_dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
            val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
            twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, sliceSize)(slice_technique)) * (current_bin_size.toDouble / num_dims)
          }).sum
        }
      } else 0

      val contrast = (head_contrast * head_MC_number + tail_contrast * tail_MC_number) / MC_num
      contrast
    }
  }

  /**
   * reference dimensions are iterated in the head and the reference dimension-dependent influences are tracked.
   * In the Tail, we first sort dimensions by their influences in the head.
   * Then we group dimensions in nearly equal-sized bins with number of bins = number of tail iterations.
   * Then we choose reference dimensions from the bins.
   * We get two estimators from both head and tail.
   * We compute their empirical variance from the tracked dimension-dependent influences.
   * We balance them by their empirical variance.
   * 30 dimensions 70 iterations => head = 30 * 2 = 60 iterations, tail = 70 % 30 = 10 iterations
   * or 30 dimensions 20 iterations => head = 0 iteration, tail = 20 % 30 = 20 iterations
   */
  def contrast_iterate_ref_dim_tail_group_ref_dim_by_influence_balance_by_empirical_variance(m: PreprocessedData, dimensions: Set[Int], MC_num: Int = M)(slice_technique: String = "c"): Double = {
    if (MC_num == 0) -1 else {

      val sliceSize = (math.pow(alpha, 1.0 / (dimensions.size - 1.0)) * m.num_obs).ceil.toInt /// WARNING: Do not forget -1

      val dims_vec = dimensions.toVector

      val dims_set = dimensions

      val num_dims = dimensions.size

      // for example 50 MC iterations with 8 dimensions: 50/8 = 6 head iterations
      val head_great_iteration_num: Int = MC_num / num_dims
      // 6*8 = 48 small MC iterations
      val head_MC_number: Int = head_great_iteration_num * num_dims
      // tail MC number is then 50 - 48 = 2
      val tail_MC_number: Int = MC_num - head_MC_number

      val head_influence_vec: Vector[Double] = if (head_MC_number >= 1) {
        if (parallelize == 0) {
          (1 to head_MC_number).map(i => {
            val referenceDim = dims_vec((i - 1) % num_dims)
            twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
          }).toVector
        } else {
          val iterations = (1 to head_MC_number).par
          if (parallelize > 1) {
            iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
          }
          iterations.map(i => {
            val referenceDim = dims_vec((i - 1) % num_dims)
            twoSample(m, referenceDim, m.slice_with_ref_dim(dims_set, referenceDim, sliceSize)(slice_technique))
          }).toVector
        }
      } else Vector(0.0) // a dumb variable that will never be used

      val head_contrast = if (head_MC_number >= 1) head_influence_vec.sum / head_MC_number else 0

      val (tail_contrast, beta) = if (tail_MC_number >= 1) {

        // if head_number = 0 => we have not got information about the influences from each dimension
        // we simply use dims_vec else, we can analyze the influence vector from the previous iterations
        // and sort the dim vec by their sum of influences
        val sorted_dims_vec = if (head_MC_number == 0) dims_vec else {
          val zipped = head_influence_vec.zipWithIndex
          (0 until num_dims).map(i => {
            val selected_vec = zipped.filter(x => x._2 % num_dims == i).map(x => x._1)
            selected_vec.sum
          }).zip(dims_vec).sortBy(x => x._1).map(x => x._2).toVector
        }

        // for example, 7 iterations left, 100 dimensions, then we have
        // (15,15,14,14,14,14,14), 7 nearly equal size bins
        val tail_bins_sizes = get_tail_bin_sizes(num_dims, tail_MC_number)

        //(15, 30, 44, 58, 72, 86, 100)
        val tail_bins_start_index = cumulative_sum(tail_bins_sizes)

        val tail_contrast = if (parallelize == 0) {
          (0 until tail_MC_number).map(x => {
            val current_bin_size = tail_bins_sizes(x)
            // crucial
            val dims_in_the_bin = sorted_dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
            val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
            // do not forget current_bin_size.toDouble!!!
            twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, sliceSize)(slice_technique)) * (current_bin_size.toDouble / num_dims)
          }).sum
        } else {
          val iterations = (0 until tail_MC_number).par
          if (parallelize > 1) {
            iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
          }
          iterations.map(x => {
            val current_bin_size = tail_bins_sizes(x)
            // crucial
            val dims_in_the_bin = sorted_dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
            val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
            twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, sliceSize)(slice_technique)) * (current_bin_size.toDouble / num_dims)
          }).sum
        }

        // beta is the Tail estimator balancing weight factor
        // beta = min( V(Head) / (V(Head) + V(Tail)), tail_MC_number / MC_num) if head_great_iteration_num greater equal a number,
        // beta = tail_MC_number / MC_num else
        val beta = if (head_great_iteration_num <= 1) tail_MC_number.toDouble / MC_num else {
          // this method gets the elements in the influence_vec where i % dims == index
          def get_ith_elements(influence_vec: Vector[Double], num_dims: Int, indices: Set[Int]): Vector[Double] = {
            influence_vec.zipWithIndex.filter(x => indices.contains(x._2 % num_dims)).map(x => x._1)
          }

          val var_head = (0 until num_dims).map(d => {
            val influences = get_ith_elements(head_influence_vec, num_dims, Set(d))
            variance(influences)
          }).sum * head_great_iteration_num / (head_MC_number * head_MC_number)

          val var_tail = tail_bins_sizes.indices.map(b => {
            val current_bin_size = tail_bins_sizes(b)
            val dims_in_the_bin = ((tail_bins_start_index(b) - current_bin_size) until tail_bins_start_index(b)).toSet
            val influences = get_ith_elements(head_influence_vec, num_dims, dims_in_the_bin)
            variance(influences) * scala.math.pow(current_bin_size.toDouble / num_dims, 2)
          }).sum

          // this happens in perfectly linear case
          if ((var_head == 0.0) & (var_tail == 0.0)) {
            tail_MC_number.toDouble / MC_num
          } else {
            (var_head / (var_head + var_tail)).min(tail_MC_number.toDouble / MC_num)
          }
        }
        (tail_contrast, beta)
      } else (0.0, 0.0)
      val contrast = (1.0 - beta) * head_contrast + beta * tail_contrast
      contrast
    }
  }

  /**
   * The generalize contrast, the core dependency estimator in GMCDE.
   * computes the dependencies between groups of dimensions.
   * It uses "ItGI" internally.
   */

  def generalized_contrast(m: PreprocessedData, groups_of_dims: Set[Set[Int]], MC_num: Int = M)(slice_technique: String = "c"): Double = {
    if (MC_num == 0) -1 else {
      // Set(Set(0,1), Set(2,3), Set(4,5,6))

      // Vector(0,1,2,3,4,5,6)
      val dim_vec = groups_of_dims.flatten.toVector

      val num_dims = dim_vec.size

      val dim_slice_size_dict = dim_vec.map(d => {
        val num_dims_to_slice = groups_of_dims.filter(g => !g.contains(d)).flatten.size
        d -> (math.pow(alpha, 1.0 / num_dims_to_slice) * m.num_obs).ceil.toInt
      }) toMap

      // Map(0 -> Set(5, 6, 2, 3, 4), 5 -> Set(0, 1, 2, 3), 1 -> Set(5, 6, 2, 3, 4),
      // 6 -> Set(0, 1, 2, 3), 2 -> Set(0, 5, 1, 6, 4), 3 -> Set(0, 5, 1, 6, 4), 4 -> Set(0, 1, 2, 3))
      val dim_slice_dimensions_dict = dim_vec.map(d => {
        d -> groups_of_dims.filter(g => !g.contains(d)).flatten
      }) toMap

      // for example 50 MC iterations with 8 dimensions: 50/8 = 6 head iterations
      val head_great_iteration_num: Int = MC_num / num_dims
      // 6*8 = 48 small MC iterations
      val head_MC_number: Int = head_great_iteration_num * num_dims
      // tail MC number is then 50 - 48 = 2
      val tail_MC_number: Int = MC_num - head_MC_number

      val head_influence_vec: Vector[Double] = if (head_MC_number >= 1) {
        if (parallelize == 0) {
          (1 to head_MC_number).map(i => {
            val referenceDim = dim_vec((i - 1) % num_dims)
            twoSample(m, referenceDim, m.slice_without_ref_dim(dim_slice_dimensions_dict(referenceDim), dim_slice_size_dict(referenceDim))(slice_technique))
          }).toVector
        } else {
          val iterations = (1 to head_MC_number).par
          if (parallelize > 1) {
            iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
          }
          iterations.map(i => {
            val referenceDim = dim_vec((i - 1) % num_dims)
            twoSample(m, referenceDim, m.slice_without_ref_dim(dim_slice_dimensions_dict(referenceDim), dim_slice_size_dict(referenceDim))(slice_technique))
          }).toVector
        }
      } else Vector(0.0) // a dumb variable that will never be used

      val head_contrast = if (head_MC_number >= 1) head_influence_vec.sum / head_MC_number else 0

      val tail_contrast = if (tail_MC_number >= 1) {

        // if head_number = 0 => we have not got information about the influences from each dimension
        // we simply use dims_vec else, we can analyze the influence vector from the previous iterations
        // and sort the dim vec by their sum of influences
        val sorted_dim_vec = if (head_MC_number == 0) dim_vec else {
          val zipped = head_influence_vec.zipWithIndex
          (0 until num_dims).map(i => {
            val selected_vec = zipped.filter(x => x._2 % num_dims == i).map(x => x._1)
            selected_vec.sum
          }).zip(dim_vec).sortBy(x => x._1).map(x => x._2).toVector
        }

        // for example, 7 iterations left, 100 dimensions, then we have
        // (15,15,14,14,14,14,14), 7 nearly equal size bins
        val tail_bins_sizes = get_tail_bin_sizes(num_dims, tail_MC_number)

        //(15, 30, 44, 58, 72, 86, 100)
        val tail_bins_start_index = cumulative_sum(tail_bins_sizes)

        if (parallelize == 0) {
          (0 until tail_MC_number).map(x => {
            val current_bin_size = tail_bins_sizes(x)
            // crucial
            val dims_in_the_bin = sorted_dim_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
            val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
            // do not forget current_bin_size.toDouble!!!
            twoSample(m, ref_dim, m.slice_without_ref_dim(dim_slice_dimensions_dict(ref_dim), dim_slice_size_dict(ref_dim))(slice_technique)) * (current_bin_size.toDouble / num_dims)
          }).sum
        } else {
          val iterations = (0 until tail_MC_number).par
          if (parallelize > 1) {
            iterations.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
          }
          iterations.map(x => {
            val current_bin_size = tail_bins_sizes(x)
            // crucial
            val dims_in_the_bin = sorted_dim_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x))
            val ref_dim = dims_in_the_bin(scala.util.Random.nextInt(current_bin_size))
            // do not forget current_bin_size.toDouble!!!
            twoSample(m, ref_dim, m.slice_without_ref_dim(dim_slice_dimensions_dict(ref_dim), dim_slice_size_dict(ref_dim))(slice_technique)) * (current_bin_size.toDouble / num_dims)
          }).sum
        }
      } else 0

      val contrast = (head_contrast * head_MC_number + tail_contrast * tail_MC_number) / MC_num
      contrast
    }
  }


}
