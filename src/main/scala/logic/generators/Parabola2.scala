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
package logic.generators

import breeze.stats.distributions.Uniform
import io.github.edouardfouche.generators.ParameterizedDataGenerator

// This generator fixes a bug in the Parabola implementation of  the io.github.edouardfouche.generators package
case class Parabola2(nDim: Int, noise: Double, noisetype: String, discretize: Int)(scale: Option[Double] = Some(1)) extends ParameterizedDataGenerator {
  override lazy val shortname = "p2"
  val s: Int = scale match {
    case Some(i) => i.asInstanceOf[Int]
    case None => 1
  }
  val param: Double = s
  val name = "parabola2"

  protected def getPoints(n: Int): Array[Array[Double]] = {
    (1 to n).toArray.map { _ =>
      var data = Array(Uniform(-1, 1).draw())
      for (y <- 2 to nDim) {
        data = data ++ Array(math.pow(data(0), 2 + (2 * (s - 1))))
      }
      data.map(x => noiselessPowerNormalization(x, 2 + (2 * (s - 1))))
    }
  }
}
