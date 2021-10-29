package logic.stats.mcde
import logic.index.RankIndex
import scala.annotation.tailrec
import scala.math.{E, pow, sqrt}

/**
 * This is a re-implementation  of the contrast measure as proposed in HiCS
 * Use the Kolmogorov-Smirnov test as basis. To the best of my knowledge, the most efficient existing implementation.
 *
 * @param alpha Expected share of instances in slice (independent dimensions).
 *
 */
//TODO: It would be actually interesting to compare MCDE with a version with the KSP-test AND all the improvements proposed by MCDE
case class KSP(M: Int = 50, alpha: Double = 0.5, var parallelize: Int = 0) extends McdeStats {
  type PreprocessedData = RankIndex
  val id = "KSP"

  /**
   * Compute the Kolmogorov Smirnov test using a reference vector (the indices of a dimension ordered by the rank) and
   * a set of Int that correspond to the intersection of the position of the element in the slices in the other
   * dimensions.
   *
   * @param reference      The original position of the elements of a reference dimension ordered by their rank
   * @param indexSelection An array of Boolean where true means the value is part of the slice
   * @return The contrast score, which is 1-p of the p-value of the Kolmogorov-Smirnov statistic
   */
  def twoSample(m: PreprocessedData, ref_dim: Int, indexSelection: Array[Boolean]): Double = {
    //require(reference.length == indexSelection.length, "reference and indexSelection should have the same size")

    val ref = m(ref_dim)
    print(ref.mkString("Array(", ", ", ")"))

    val inSlize = indexSelection.count(_ == true)
    val outSlize = ref.length - inSlize

    if (inSlize == 0 || outSlize == 0) return 1.0 // If one is empty they are perfectly diffrent --> score = 1 (and no prob with division by 0)

    val selectIncrement = 1.0 / inSlize
    val refIncrement = 1.0 / outSlize


    // This step is impossible (or difficult) to parallelize, but at least it is tail recursive
    @tailrec def cumulative(n: Int, acc1: Double, acc2: Double, currentMax: Double): Double = {
      if (n == ref.length) currentMax
      else {
        if (indexSelection(ref(n)))
          cumulative(n + 1, acc1 + selectIncrement, acc2, currentMax max math.abs(acc2 - (acc1 + selectIncrement)))
        else
          cumulative(n + 1, acc1, acc2 + refIncrement, currentMax max math.abs(acc2 + refIncrement - acc1))
      }
    }

    val D = cumulative(0, 0, 0, 0)
    val p = get_p_from_D(D, inSlize, outSlize)
    p
  }

    /**
     * Convert the D value into a p-value
     *
     * @param D  D value from KSP test
     * @param n1 n Datapoints in first sample
     * @param n2 n Datapoints in second sample
     * @return p-value of two-sided two-sample KSP
     */
    def get_p_from_D(D: Double, n1: Long, n2: Long): Double = {
      lazy val z = D * sqrt(n1 * n2 / (n1 + n2))

      def exp(k: Int): Double = pow(-1, k - 1) * pow(E, -2 * pow(k, 2) * pow(z, 2))

      def infi_exp(k: Int): Double = pow(-1, k-1) * pow(E, 2 * pow(k,2) * pow(D, 2)) // in case lim n1, n2 -> infi
      // we take positive power instead of negative power because it overflows when (n1 >= 3037000499L && n2 >= 3037000499L)

      // TODO: The part inside the summation could be done easily in parallel
      @tailrec
      def loop(sumation: Double, i: Int, end: Int, f: Int => Double): Double = {
        if (i == end) f(i) + sumation
        else loop(f(i) + sumation, i + 1, end, f)
      }


      if(n1 >= 3037000499L && n2 >= 3037000499L) 1 - 2 * loop(0, 1, 1000, infi_exp) // squaring n1,n2 will reach the limit of Long
      else 1 - 2 * loop(0, 1, 1000, exp)
  }
}
