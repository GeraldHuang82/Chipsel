// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental

import chisel3._
import chisel3.internal.ViewBinding.{Direct, ViewTarget}
import chisel3.internal.{AggregateViewBinding, requireIsChiselType, ViewBinding}

import java.util
import scala.collection.mutable

package object dataview {
  // TODO should this be moved to class Aggregate / can it be unified with Aggregate.bind?
  private def bindAgg[A <: Data, B <: Aggregate](a: A, b: B, mapping: Iterable[(Data, Data)]): Unit = {
    println(s"Mapping = ${mapping.map { case (x, y) => (x -> x._id) -> (y -> y._id) }}")
    //val viewFields = getRecursiveFields(result, "(bundle root)").toMap

    // We rely on hashCode and equality based on identity here, consider IdentityHashMap if that changes
    val childBindings = mutable.HashMap(b.getElements.map(_ -> new mutable.ListBuffer[ViewTarget]):_*)
      //[Data, mutable.ListBuffer[ViewTarget]]()
    //b.getElements.foreach(childBindings.put(_, new mutable.ListBuffer)) // Java is dumb
    for ((ax, bx) <- mapping) {
      println(s"Looking up ${bx -> bx._id}")
      // TODO handle ax and bx not being fields of a or b
      childBindings(bx) += Direct(ax)
    }

    val resultBindings = childBindings.view.map { case (data, targets) =>
      val targetsx = targets match {
        case collection.Seq(target: Direct) => target
        case x =>
          throw new Exception(s"Got $x, expected Seq(_: Direct)")
      }
      data -> targetsx
    }.toMap
    b.bind(AggregateViewBinding(resultBindings))
  }

  private def bindElt[A <: Data, B <: Element](a: A, b: B, mapping: Iterable[(Data, Data)]): Unit = {
    mapping.toList match {
      case (ax, `b`) :: Nil => b.bind(ViewBinding(Direct(ax)))
      case other => throw new Exception(s"Expected exactly 1 mapping, got $other")
    }
  }

  // TODO is this right place to put this?
  implicit class DataViewable[A <: Data](a: A) {
    def viewAs[B <: Data](b: B)(implicit view: DataView[A, B]): B = {
      requireIsHardware(a, "viewAs")
      requireIsChiselType(b, "viewAs")
      val result: B = b.cloneTypeFull

      val mapping = view.mapping.map(f => f(a, result))
      result match {
        case agg: Aggregate =>
          println(s"agg.getElements = ${agg.getElements.map(x => x -> x._id)}")
          bindAgg(a, agg, mapping)
        case elt: Element =>
          bindElt(a, elt, mapping)
      }
      result
    }
  }
}
