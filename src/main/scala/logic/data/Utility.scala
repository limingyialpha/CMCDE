package logic.data

import scala.collection.immutable._


object Utility {

  def cumulative_average(vec: Vector[Double]): Vector[Double]={
    vec.scanLeft(0.0)(_+_).drop(1).zip(1 to vec.length).map(x =>x._1/x._2)
  }
}
