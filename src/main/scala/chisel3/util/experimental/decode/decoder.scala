// SPDX-License-Identifier: Apache-2.0

package chisel3.util.experimental.decode

import chisel3._
import chisel3.util.experimental.{getAnnotations, pla}
import logger.LazyLogging

object decoder extends LazyLogging {
  private val caches = getAnnotations().collect {
    case DecodeTableAnnotation(_, in, out) => in -> out
  }.toMap

  def apply(minimizer: Minimizer, input: UInt, truthTable: TruthTable): UInt = {
    val minimizedTable = caches.getOrElse(
      truthTable, {
        val startTime = System.nanoTime()
        val minimizedTable = minimizer.minimize(truthTable)
        val totalTime = System.nanoTime() - startTime
        val totalTimeInSeconds = totalTime / 1e6
        val info = f"Logic Minimize with $minimizer finished in ${totalTimeInSeconds} second"
        if (totalTimeInSeconds > 120)
          logger.error(
            s"$info spends too long, consider use chisel3.util.experimental.DecodeTableAnnotation to cache decode result."
          )
        else logger.trace(info)
        minimizedTable
      }
    )
    if (minimizedTable.table.isEmpty) {
      val outputs = Wire(UInt(minimizedTable.default.getWidth.W))
      outputs := minimizedTable.default.value.U(minimizedTable.default.getWidth.W)
      outputs
    } else {
      val (plaInput, plaOutput) = pla(minimizedTable.table.toSeq, minimizedTable.default.value.U)
//      @todo bug in DecodeTableAnnotation, need (de)serialization 
//      annotate(new ChiselAnnotation {
//        override def toFirrtl: Annotation =
//          DecodeTableAnnotation(plaOutput.toTarget, truthTable, minimizedTable)
//      })

      plaInput := input
      plaOutput
    }
  }
}
