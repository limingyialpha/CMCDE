package logic.stats.mcde

import scala.collection.parallel.ForkJoinTaskSupport
import logic.index.Index
import logic.data.Utility.cumulative_sum

import scala.collection.parallel.immutable.ParRange

// breeze.stats.variance computes the empirical variance
import breeze.stats.variance

import scala.language.postfixOps

trait McdeStats {
  type PreprocessedData <: Index
  val id: String
  // alpha is the expected share of instances in slice.
  val alpha: Double
  // Iteration number for contrast / generalized contrast approximation.
  val num_iterations: Int
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
  def contrast(m: PreprocessedData, dims_set: Set[Int], num_iterations: Int = num_iterations)(estimator: String = "R", slice_technique: String = "c"): Double = {
    estimator match {
      case "R" => contrast_random_ref_dim(m, dims_set, num_iterations)(slice_technique)
      case "ItR" => contrast_iterate_ref_dim_tail_random(m, dims_set, num_iterations)(slice_technique)
      case "ItGR" => contrast_iterate_ref_dim_tail_group_ref_dim_randomly(m, dims_set, num_iterations)(slice_technique)
      case "ItGI" => contrast_iterate_ref_dim_tail_group_ref_dim_by_influence(m, dims_set, num_iterations)(slice_technique)
      case "ItGIBEV" => contrast_iterate_ref_dim_tail_group_ref_dim_by_influence_balance_by_empirical_variance(m, dims_set, num_iterations)(slice_technique)
    }
  }

  def get_range(end: Int): Range = {
    0 until end
  }

  def get_par_range(end: Int): ParRange = {
    val iterable = (0 until end).par
    if (parallelize > 1) {
      iterable.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(parallelize))
    }
    iterable
  }

  def get_slice_size_with_ref_dim(dims_set: Set[Int], num_obs: Int): Int = {
    (math.pow(alpha, 1.0 / (dims_set.size - 1.0)) * num_obs).ceil.toInt
  }

  def get_slice_size_without_ref_dim(dims_set: Set[Int], num_obs: Int): Int = {
    (math.pow(alpha, 1.0 / dims_set.size) * num_obs).ceil.toInt
  }

  def get_num_head_great_iterations(num_dims: Int, num_iterations: Int): Int = {
    num_iterations / num_dims
  }

  def get_num_head_iterations(num_dims: Int, num_iterations: Int): Int = {
    num_iterations / num_dims * num_dims
  }

  def get_num_tail_iterations(num_dims: Int, num_iterations: Int): Int = {
    num_iterations - num_iterations / num_dims * num_dims
  }

  def get_random_ref_dim(dims_vec: Vector[Int]): Int = {
    dims_vec(scala.util.Random.nextInt(dims_vec.size))
  }

  def get_itvs_by_R(m: PreprocessedData, dims_set: Set[Int], num_iterations: Int)(slice_technique: String = "c"): Vector[Double] = {
    val slice_size = get_slice_size_with_ref_dim(dims_set, m.num_obs)
    val dims_vec = dims_set.toVector

    val itvs = {
      if (parallelize == 0) {
        val range = get_range(num_iterations)
        range.map(_ => {
          val ref_dim = get_random_ref_dim(dims_vec)
          twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, slice_size)(slice_technique))
        }).toVector
      } else {
        val par_range = get_par_range(num_iterations)
        par_range.map(_ => {
          val ref_dim = get_random_ref_dim(dims_vec)
          twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, slice_size)(slice_technique))
        }).toVector
      }
    }
    itvs
  }

  def get_itv_ref_dim_pairs_by_I(m: PreprocessedData, dims_set: Set[Int], num_great_iterations: Int)(slice_technique: String = "c"): Vector[(Double, Int)] = {
    val num_dims = dims_set.size
    val slice_size = get_slice_size_with_ref_dim(dims_set, m.num_obs)
    val dims_vec = dims_set.toVector

    val num_iterations = num_great_iterations * num_dims

    val itv_ref_dim_pairs = {
      if (parallelize == 0) {
        val range = get_range(num_iterations)
        range.map(k => {
          val ref_dim = dims_vec(k % num_dims)
          (twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, slice_size)(slice_technique)), ref_dim)
        }).toVector
      } else {
        val par_range = get_par_range(num_iterations)
        par_range.map(k => {
          val ref_dim = dims_vec(k % num_dims)
          (twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, slice_size)(slice_technique)), ref_dim)
        }).toVector
      }
    }
    itv_ref_dim_pairs
  }

  def get_itvs_by_I(m: PreprocessedData, dims_set: Set[Int], num_great_iterations: Int)(slice_technique: String = "c"): Vector[Double] = {
    get_itv_ref_dim_pairs_by_I(m, dims_set, num_great_iterations)(slice_technique).map(x => x._1)
  }

  def get_itv_dims_bin_pairs_from_bins(m: PreprocessedData, dims_bins: Set[Set[Int]])(slice_technique: String = "c"): Vector[(Double, Set[Int])] = {
    val dims_set = dims_bins.flatten
    val slice_size = get_slice_size_with_ref_dim(dims_set, m.num_obs)

    val itv_dims_bin_pair = {
      if (parallelize == 0) {
        val range = dims_bins
        range.map(bin => {
          val bin_vec = bin.toVector
          val ref_dim = get_random_ref_dim(bin_vec)
          (twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, slice_size)(slice_technique)), bin)
        }).toVector
      } else {
        val par_range = dims_bins.par
        par_range.map(bin => {
          val bin_vec = bin.toVector
          val ref_dim = get_random_ref_dim(bin_vec)
          (twoSample(m, ref_dim, m.slice_with_ref_dim(dims_set, ref_dim, slice_size)(slice_technique)), bin)
        }).toVector
      }
    }
    itv_dims_bin_pair
  }

  def get_itvs_from_bins(m: PreprocessedData, dims_bins: Set[Set[Int]])(slice_technique: String = "c"): Vector[Double] = {
    get_itv_dims_bin_pairs_from_bins(m, dims_bins)(slice_technique).map(x => x._1)
  }

  def get_tail_bins(dims_set: Set[Int], num_tail_iterations: Int): Set[Set[Int]] = {
    val num_dims = dims_set.size
    val dims_vec = dims_set.toVector

    val tail_bins_sizes = (0 until num_tail_iterations).map(x => {
      val overflow = if (x < num_dims % num_tail_iterations) 1 else 0
      overflow + num_dims / num_tail_iterations
    }).toVector
    val tail_bins_start_index = cumulative_sum(tail_bins_sizes)
    (0 until num_tail_iterations).map(x => {
      val current_bin_size = tail_bins_sizes(x)
      dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x)).toSet
    }).toSet
  }

  def get_tail_bins_by_influence(dims_set: Set[Int], num_tail_iterations: Int, head_itv_ref_dim_pairs: Vector[(Double, Int)]): Set[Set[Int]] = {
    val num_dims = dims_set.size

    val sorted_dims_vec = {
      (0 until num_dims).map(i => {
        val selected_values = head_itv_ref_dim_pairs.filter(x => x._2 == i).map(x => x._1)
        (selected_values.sum, i)
      }).sortBy(x => x._1).map(x => x._2).toVector
    }

    val tail_bins_sizes = (0 until num_tail_iterations).map(x => {
      val overflow = if (x < num_dims % num_tail_iterations) 1 else 0
      overflow + num_dims / num_tail_iterations
    }).toVector
    val tail_bins_start_index = cumulative_sum(tail_bins_sizes)
    (0 until num_tail_iterations).map(x => {
      val current_bin_size = tail_bins_sizes(x)
      sorted_dims_vec.slice(tail_bins_start_index(x) - current_bin_size, tail_bins_start_index(x)).toSet
    }).toSet
  }

  def get_empirical_var(bin: Set[Int], head_itv_ref_dim_pairs: Vector[(Double, Int)]): Double = {
    val values = head_itv_ref_dim_pairs.filter(x => bin.contains(x._2)).map(x => x._1)
    variance(values)
  }

  /**
   * Original dependency estimator in MCDE where reference dimensions are randomly chosen
   * the true Monte Carlo
   */
  def contrast_random_ref_dim(m: PreprocessedData, dims_set: Set[Int], num_iterations: Int = num_iterations)(slice_technique: String = "c"): Double = {
    if (num_iterations == 0) {
      -1
    } else {
      get_itvs_by_R(m, dims_set, num_iterations)(slice_technique).sum / num_iterations
    }
  }

  /**
   * reference dimensions are iterated in the head. In the Tail, we choose reference dimensions randomly.
   * We get two estimators from both head and tail, and we balance them with their iteration numbers.
   * 30 dimensions 70 iterations => head = 30 * 2 = 60 iterations, tail = 70 % 30 = 10 iterations
   * or 30 dimensions 20 iterations => head = 0 iteration, tail = 20 % 30 = 20 iterations
   */
  def contrast_iterate_ref_dim_tail_random(m: PreprocessedData, dims_set: Set[Int], num_iterations: Int = num_iterations)(slice_technique: String = "c"): Double = {
    val num_dims = dims_set.size
    val num_head_great_iterations = get_num_head_great_iterations(num_dims, num_iterations)
    val num_tail_iterations = get_num_tail_iterations(num_dims, num_iterations)

    if (num_iterations == 0) {
      -1.0
    } else if (num_head_great_iterations != 0 & num_tail_iterations == 0) {
      get_itvs_by_I(m, dims_set, num_head_great_iterations)(slice_technique).sum / num_iterations
    } else if (num_head_great_iterations == 0 & num_tail_iterations != 0) {
      get_itvs_by_R(m, dims_set, num_tail_iterations)(slice_technique).sum / num_iterations
    } else {
      val head_itvs = get_itvs_by_I(m, dims_set, num_head_great_iterations)(slice_technique)
      val tail_itvs = get_itvs_by_R(m, dims_set, num_tail_iterations)(slice_technique)
      (head_itvs.sum + tail_itvs.sum) / num_iterations
    }
  }

  /**
   * reference dimensions are iterated in the head. In the Tail, we group dimensions randomly in nearly equal-sized bins
   * with number of bins = number of tail iterations. Then we choose reference dimensions from the bins.
   * We get two estimators from both head and tail, and we balance them with their iteration numbers.
   * 30 dimensions 70 iterations => head = 30 * 2 = 60 iterations, tail = 70 % 30 = 10 iterations
   * or 30 dimensions 20 iterations => head = 0 iteration, tail = 20 % 30 = 20 iterations
   */
  def contrast_iterate_ref_dim_tail_group_ref_dim_randomly(m: PreprocessedData, dims_set: Set[Int], num_iterations: Int = num_iterations)(slice_technique: String = "c"): Double = {
    val num_dims = dims_set.size
    val num_head_great_iterations = get_num_head_great_iterations(num_dims, num_iterations)
    val num_tail_iterations = get_num_tail_iterations(num_dims, num_iterations)
    println(num_tail_iterations)

    if (num_iterations == 0) {
      -1.0
    } else if (num_head_great_iterations != 0 & num_tail_iterations == 0) {
      get_itvs_by_I(m, dims_set, num_head_great_iterations)(slice_technique).sum / num_iterations
    } else if (num_head_great_iterations == 0 & num_tail_iterations != 0) {
      val tail_bins = get_tail_bins(dims_set, num_tail_iterations)
      get_itvs_from_bins(m, tail_bins)(slice_technique).sum / num_iterations
    } else {
      val head_itvs = get_itvs_by_I(m, dims_set, num_head_great_iterations)(slice_technique)
      val tail_bins = get_tail_bins(dims_set, num_tail_iterations)
      println(tail_bins)
      val tail_itvs = get_itvs_from_bins(m, tail_bins)(slice_technique)
      println(tail_itvs)
      (head_itvs.sum + tail_itvs.sum) / num_iterations
    }
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
  def contrast_iterate_ref_dim_tail_group_ref_dim_by_influence(m: PreprocessedData, dims_set: Set[Int], num_iterations: Int = num_iterations)(slice_technique: String = "c"): Double = {
    val num_dims = dims_set.size
    val num_head_great_iterations = get_num_head_great_iterations(num_dims, num_iterations)
    val num_tail_iterations = get_num_tail_iterations(num_dims, num_iterations)

    if (num_iterations == 0) {
      -1.0
    } else if (num_head_great_iterations != 0 & num_tail_iterations == 0) {
      get_itvs_by_I(m, dims_set, num_head_great_iterations)(slice_technique).sum / num_iterations
    } else if (num_head_great_iterations == 0 & num_tail_iterations != 0) {
      val tail_bins = get_tail_bins(dims_set, num_tail_iterations)
      get_itvs_from_bins(m, tail_bins)(slice_technique).sum / num_iterations
    } else {
      val head_itv_ref_dim_pairs = get_itv_ref_dim_pairs_by_I(m, dims_set, num_head_great_iterations)(slice_technique)
      val head_itvs = head_itv_ref_dim_pairs.map(x => x._1)
      val tail_bins = get_tail_bins_by_influence(dims_set, num_tail_iterations, head_itv_ref_dim_pairs)
      println(tail_bins)
      val tail_itvs = get_itvs_from_bins(m, tail_bins)(slice_technique)
      println(tail_itvs)
      (head_itvs.sum + tail_itvs.sum) / num_iterations
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
  def contrast_iterate_ref_dim_tail_group_ref_dim_by_influence_balance_by_empirical_variance(m: PreprocessedData, dims_set: Set[Int], num_iterations: Int = num_iterations)(slice_technique: String = "c"): Double = {
    val num_dims = dims_set.size
    val num_head_great_iterations = get_num_head_great_iterations(num_dims, num_iterations)
    val num_head_iterations = get_num_head_iterations(num_dims, num_iterations)
    val num_tail_iterations = get_num_tail_iterations(num_dims, num_iterations)

    if (num_iterations == 0) {
      -1.0
    } else if (num_head_great_iterations != 0 & num_tail_iterations == 0) {
      get_itvs_by_I(m, dims_set, num_head_great_iterations)(slice_technique).sum / num_iterations
    } else if (num_head_great_iterations == 0 & num_tail_iterations != 0) {
      val tail_bins = get_tail_bins(dims_set, num_tail_iterations)
      get_itvs_from_bins(m, tail_bins)(slice_technique).sum / num_iterations
    } else if (num_head_great_iterations <= 1 & num_tail_iterations != 0) {
      val head_itv_ref_dim_pairs = get_itv_ref_dim_pairs_by_I(m, dims_set, num_head_great_iterations)(slice_technique)
      val head_itvs = head_itv_ref_dim_pairs.map(x => x._1)
      val tail_bins = get_tail_bins_by_influence(dims_set, num_tail_iterations, head_itv_ref_dim_pairs)
      val tail_itvs = get_itvs_from_bins(m, tail_bins)(slice_technique)
      (head_itvs.sum + tail_itvs.sum) / num_iterations
    } else {
      val head_itv_ref_dim_pairs = get_itv_ref_dim_pairs_by_I(m, dims_set, num_head_great_iterations)(slice_technique)
      val head_itvs = head_itv_ref_dim_pairs.map(x => x._1)
      val head_contrast = head_itvs.sum / num_head_iterations

      val tail_bins = get_tail_bins_by_influence(dims_set, num_tail_iterations, head_itv_ref_dim_pairs)
      val tail_itv_dims_bin_pairs = get_itv_dims_bin_pairs_from_bins(m, tail_bins)(slice_technique)
      val tail_contrast = tail_itv_dims_bin_pairs.map(pair => pair._1 * pair._2.size / num_dims).sum

      val head_empirical_var = dims_set.map(dim => get_empirical_var(Set(dim), head_itv_ref_dim_pairs)).sum * num_head_great_iterations / math.pow(num_head_iterations, 2)
      val tail_empirical_var = tail_bins.map(bin => get_empirical_var(bin, head_itv_ref_dim_pairs) * math.pow(bin.size / num_dims, 2)).sum

      //we deal with perfectly linearly correlation here
      val contrast = if (head_empirical_var == 0.0 | tail_empirical_var == 0.0) {
        (head_contrast * num_head_iterations + tail_contrast * num_tail_iterations) / num_iterations
      } else {
        // beta is the Tail estimator balancing weight factor
        // beta = min( (V(Head) / (V(Head) + V(Tail)), num_tail_iterations / num_iterations)
        // For safety, we should not underestimate the variance of the tail, thus we should take the minimum
        val beta = (num_tail_iterations.toDouble / num_iterations).min(head_empirical_var / (head_empirical_var + tail_empirical_var))
        (1 - beta) * head_contrast + beta * tail_contrast
      }
      contrast
    }
  }

  def get_dim_slice_size_dict(m: PreprocessedData, groups_of_dims: Set[Set[Int]]): Map[Int, Int] = {
    groups_of_dims.flatten.map(d => {
      val dims_to_slice = groups_of_dims.filter(g => !g.contains(d)).flatten
      d -> get_slice_size_without_ref_dim(dims_to_slice, m.num_obs)
    }) toMap
  }

  def get_dim_slice_dims_dict(groups_of_dims: Set[Set[Int]]): Map[Int, Set[Int]] = {
    groups_of_dims.flatten.map(d => {
      d -> groups_of_dims.filter(g => !g.contains(d)).flatten
    }) toMap
  }

  /**
   * The generalize contrast, the core dependency estimator in GMCDE.
   * computes the dependencies between groups of dimensions.
   * It uses "ItGI" internally.
   */
  def generalized_contrast(m: PreprocessedData, groups_of_dims: Set[Set[Int]], num_iterations: Int = num_iterations)(slice_technique: String = "c"): Double = {
    val dims_vec = groups_of_dims.flatten.toVector
    val dims_set = groups_of_dims.flatten
    val num_dims = dims_set.size
    val num_head_great_iterations = get_num_head_great_iterations(num_dims, num_iterations)
    val num_tail_iterations = get_num_tail_iterations(num_dims, num_iterations)

    val dim_ss_dict = get_dim_slice_size_dict(m, groups_of_dims)
    val dim_sdims_dict = get_dim_slice_dims_dict(groups_of_dims)

    def get_itv_ref_dim_pairs_by_I_inner(num_great_iterations: Int): Vector[(Double, Int)] = {
      val num_total_iterations = num_great_iterations * num_dims
      val itv_ref_dim_pairs = {
        if (parallelize == 0) {
          val range = get_range(num_total_iterations)
          range.map(k => {
            val ref_dim = dims_vec(k % num_dims)
            val ss = dim_ss_dict(ref_dim)
            val sdims = dim_sdims_dict(ref_dim)
            (twoSample(m, ref_dim, m.slice_without_ref_dim(sdims, ss)(slice_technique)), ref_dim)
          }).toVector
        } else {
          val par_range = get_par_range(num_total_iterations)
          par_range.map(k => {
            val ref_dim = dims_vec(k % num_dims)
            val ss = dim_ss_dict(ref_dim)
            val sdims = dim_sdims_dict(ref_dim)
            (twoSample(m, ref_dim, m.slice_without_ref_dim(sdims, ss)(slice_technique)), ref_dim)
          }).toVector
        }
      }
      itv_ref_dim_pairs
    }

    def get_itvs_by_I_inner(num_great_iterations: Int): Vector[Double] = {
      get_itv_ref_dim_pairs_by_I_inner(num_great_iterations).map(x => x._1)
    }

    def get_itv_dims_bin_pairs_from_bins_inner(dims_bins: Set[Set[Int]]): Vector[(Double, Set[Int])] = {
      val itv_dims_bin_pair = {
        if (parallelize == 0) {
          val range = dims_bins
          range.map(bin => {
            val bin_vec = bin.toVector
            val ref_dim = get_random_ref_dim(bin_vec)
            val ss = dim_ss_dict(ref_dim)
            val sdims = dim_sdims_dict(ref_dim)
            (twoSample(m, ref_dim, m.slice_without_ref_dim(sdims, ss)(slice_technique)), bin)
          }).toVector
        } else {
          val par_range = dims_bins.par
          par_range.map(bin => {
            val bin_vec = bin.toVector
            val ref_dim = get_random_ref_dim(bin_vec)
            val ss = dim_ss_dict(ref_dim)
            val sdims = dim_sdims_dict(ref_dim)
            (twoSample(m, ref_dim, m.slice_without_ref_dim(sdims, ss)(slice_technique)), bin)
          }).toVector
        }
      }
      itv_dims_bin_pair
    }

    def get_itvs_from_bins_inner(dims_bins: Set[Set[Int]]): Vector[Double] = {
      get_itv_dims_bin_pairs_from_bins_inner(dims_bins).map(x => x._1)
    }

    if (num_iterations == 0) {
      -1.0
    } else if (num_head_great_iterations != 0 & num_tail_iterations == 0) {
      get_itvs_by_I_inner(num_head_great_iterations).sum / num_iterations
    } else if (num_head_great_iterations == 0 & num_tail_iterations != 0) {
      val tail_bins = get_tail_bins(dims_set, num_tail_iterations)
      get_itvs_from_bins_inner(tail_bins).sum / num_iterations
    } else {
      val head_itv_ref_dim_pairs = get_itv_ref_dim_pairs_by_I_inner(num_head_great_iterations)
      val head_itvs = head_itv_ref_dim_pairs.map(x => x._1)
      val tail_bins = get_tail_bins_by_influence(dims_set, num_tail_iterations, head_itv_ref_dim_pairs)
      val tail_itvs = get_itvs_from_bins_inner(tail_bins)
      (head_itvs.sum + tail_itvs.sum) / num_iterations
    }
  }
}
