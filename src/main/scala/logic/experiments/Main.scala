package logic.experiments

import com.typesafe.scalalogging.LazyLogging

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    ContrastApproximationConvergenceSpeed.run()
  }
}