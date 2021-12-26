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
import java.io.{BufferedWriter, File, FileWriter}

import scala.annotation.tailrec

/**
  * Created by fouchee on 11.07.17.
  */
package object utils {
  // expect rows
  def saveDataSet[T](res: Array[Array[T]], path: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s"${(1 to res(0).length) mkString ","} \n") // a little header
    res.foreach(x => bw.write(s"${x mkString ","} \n"))
    bw.close()
  }


  def save[T](res: Array[T], path: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(res mkString ",")
    bw.close()
  }

  def createFolderIfNotExisting(path: String): Unit = {
    val directory = new File(path)
    if (!directory.exists()) {
      directory.mkdir()
    }
  }
}
