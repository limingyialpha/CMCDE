/*
 * Copyright (C) 2020 Edouard Fouché
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
package logic.experiments

import com.typesafe.scalalogging.LazyLogging
import io.github.edouardfouche.generators._
import org.slf4j.MDC
import logic.utils.StopWatch

import java.io.{File, FileWriter}

/**
  * Created by fouchee on 26.07.17.
  */
abstract class Experiment(output_folder: String) extends LazyLogging {
  // output formatting
  val master_experiment_folder: String = output_folder concat "/" concat "experiments_output"
  utils.createFolderIfNotExisting(master_experiment_folder)
  val formatter = new java.text.SimpleDateFormat("yyy-MM-dd-HH-mm")
  val dirname: String = s"${formatter.format(java.util.Calendar.getInstance().getTime)}_${this.getClass.getSimpleName}_scala"
  val experiment_folder: String = master_experiment_folder concat "/" concat dirname
  val summaryPath: String = experiment_folder + "/" + this.getClass.getSimpleName + "_scala" + ".csv"

  MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName}_scala")

  info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting the experiment ${this.getClass.getSimpleName}\n")
  utils.createFolderIfNotExisting(experiment_folder)

  info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

  def run(): Unit

  def info(s: String): Unit = {
    // Repeat the MDC so that we are sure that, even if we are in a subprocess, that the information will be logged centrally
    MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName}_scala")
    logger.info(s)
  }

  case class ExperimentSummary(attributes: List[String]) {
    val results: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()

    def add(name: String, v: Any): Unit = {
      results(name) = v
    }

    // this is used for well categorized data
    def write(path: String): Unit = {
      synchronized {
        if(!new File(path).exists) { // write the header
          val fileA = new File(path)
          val fwA = new FileWriter(fileA, true)
          fwA.write(getHeader)
          fwA.flush()
          fwA.close()
        }
        val fileA = new File(path)
        val fwA = new FileWriter(fileA, true) // append set to true
        fwA.write(this.toString) // this is the string
        fwA.flush()
        fwA.close()
      }
    }

    // this is used for storing a large number of data of the same type
    def direct_write(path:String, content:String): Unit = {
      synchronized {
        if(!new File(path).exists) { // write the header
          val fileA = new File(path)
          val fwA = new FileWriter(fileA, true)
          fwA.write(getHeader)
          fwA.flush()
          fwA.close()
        }
        val fileA = new File(path)
        val fwA = new FileWriter(fileA, true) // append set to true
        fwA.write(content + "\n") // this is the string
        fwA.flush()
        fwA.close()
      }
    }

    override def toString: String = {
      (attributes.map(x => results.getOrElse(x, "NULL").toString) mkString ",") + "\n"
    }

    def getHeader: String = (attributes mkString ",") + "\n"
  }
}
