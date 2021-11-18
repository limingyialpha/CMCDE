package logic.data

import scala.collection.immutable._


object Utility {

  def cumulative_average(vec: Vector[Double]): Vector[Double]={
    vec.scanLeft(0.0)(_+_).drop(1).zip(1 to vec.length).map(x =>x._1/x._2)
  }

  def cumulative_sum(vec: Vector[Int]): Vector[Int] = {
    vec.scanLeft(0)(_+_).drop(1)
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x-m, 2))))
  }

  def empirical_variance(xs: Seq[Double]): Double = {
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x-m, 2)))).get * xs.length / (xs.length - 1)
  }

  // round(3.25235252, 2) ->  3.25
  def round(v: Double, digits: Int): Double = {
    scala.math.round(v * scala.math.pow(10, digits)).toDouble / scala.math.pow(10, digits)
  }
}
