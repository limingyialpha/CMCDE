package logic.experiments

import com.typesafe.scalalogging.LazyLogging

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    iter_vs_iteruni_my_vs_iteruni_edouard.run()
  }
}