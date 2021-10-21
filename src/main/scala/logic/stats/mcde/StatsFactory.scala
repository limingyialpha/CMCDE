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
package logic.stats.mcde

object StatsFactory {
  def getTest(test: String, m: Int, alpha: Double, beta: Double, parallelize: Int): Stats =
  test.toLowerCase match {
    case "mcdep" => MCDEP(m, alpha, beta, parallelize)
    case "cspmr" => CSPmr(m, alpha, beta, parallelize)
    case "csp" => CSP(m, alpha, beta, parallelize)
    case "kspemr" => KSPemr(m, alpha, beta, parallelize)
    case "kspe" => KSPe(m, alpha, beta, parallelize)
    case "kspmr" => KSPmr(m, alpha, beta, parallelize)
    case "ksp" => KSP(m, alpha, beta, parallelize)
    case "mwp" => MWP(m, alpha, beta, parallelize)
    case "mwpnomr" => MWPnomr(m, alpha, beta, parallelize)
    case "mwpr" => MWPr(m, alpha, beta, parallelize)
    case "mwpu" => MWPu(m, alpha, beta, parallelize)
    case _ => throw new Error(s"Unknown statistical test $test")
  }
}
