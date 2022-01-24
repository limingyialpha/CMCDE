package logic.data

import breeze.linalg.DenseMatrix

import scala.collection.immutable._

/**
 * Provide utilities for number, vector, matrix computation or manipulation
 */
object Utility {

  def cumulative_average(vec: Vector[Double]): Vector[Double]={
    vec.scanLeft(0.0)(_+_).drop(1).zip(1 to vec.length).map(x =>x._1/x._2)
  }

  def cumulative_sum(vec: Vector[Int]): Vector[Int] = {
    vec.scanLeft(0)(_+_).drop(1)
  }

  // round(3.25235252, 2) ->  3.25
  def round(v: Double, digits: Int): Double = {
    scala.math.round(v * scala.math.pow(10, digits)).toDouble / scala.math.pow(10, digits)
  }

  def get_submtx(mtx: Array[Array[Double]], set_dims: Set[Int]): Array[Array[Double]] = {
    mtx.map(row => row.zipWithIndex.filter(ele => set_dims.contains(ele._2)).map(ele => ele._1))
  }

  def get_submtx_as_densemtx(mtx: Array[Array[Double]], set_dims: Set[Int]): DenseMatrix[Double] = {
    val submtx = mtx.map(row => row.zipWithIndex.filter(ele => set_dims.contains(ele._2)).map(ele => ele._1))
    DenseMatrix(submtx: _*)
  }
}
