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
    // Lookups to check the mapping results
    val viewFieldLookup = getRecursiveFields(b, "(aggregate root)").toMap
    val targetFieldLookup = getRecursiveFields(a, "(target root)").toMap

    // Resulting bindings for each Element
    val childBindings =
      new mutable.HashMap[Data, mutable.ListBuffer[ViewTarget]] ++
        viewFieldLookup.view
          .collect { case (elt: Element, _) => elt }
          .map(_ -> new mutable.ListBuffer[ViewTarget])

    for ((ax, bx) <- mapping) {
      def err(arg: Data) =
        throw new Exception(s"View mapping must only contain Elements within the two types, got $arg")
      val fieldName = viewFieldLookup.getOrElse(bx, err(bx))
      targetFieldLookup.getOrElse(ax, err(ax))

      bx match {
        // Special cased because getMatchedFields checks typeEquivalence on Elements (and is used in Aggregate path)
        // Also saves object allocations on common case of Elements
        case elt: Element =>
          if (elt.getClass != ax.getClass) {  // TODO typeEquivalent is too strict because it checks width
            throw new Exception(s"Field $fieldName $elt specified as view of non-type-equivalent value $ax")
          }
          childBindings(bx) += Direct(ax)

        case agg: Aggregate =>
          if (!agg.typeEquivalent(ax)) {
            throw new Exception(s"field $fieldName $agg specified with non-type-equivalent value $ax")
          }
          getMatchedFields(agg, ax).foreach {
            case (belt: Element, aelt: Element) =>
              childBindings(belt) += Direct(aelt)
            case _ => // Ignore matching of Aggregates
          }
      }
    }

    val resultBindings = childBindings.map { case (data, targets) =>
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
          bindAgg(a, agg, mapping)
        case elt: Element =>
          bindElt(a, elt, mapping)
      }
      result
    }
  }
}
