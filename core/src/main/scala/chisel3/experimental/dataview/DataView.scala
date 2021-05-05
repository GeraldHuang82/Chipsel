// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.dataview
import chisel3._
import chisel3.internal.ViewBinding

// TODO can this be a trait?
// TODO what about views as the same type? What happens if the width or parameters are different?
final class DataView[A <: Data, B <: Data] private (private[chisel3] val mapping: Iterable[((A, B) => (Data, Data))])

object DataView {
  // TODO, return tuple of views, one for each direction?
  // Possible alternatives to implicitly swapping mapping direction
  //  implicit val bundleBToA = bundleAToB.flip
  //  implicit val (bToA, aToB) = DataView[BundleA, BundleB].bimap(_.foo -> _.buzz, _.bar -> _.fizz)
  def apply[A <: Data, B <: Data](mapping: ((A, B) => (Data, Data))*): DataView[A, B] = {
    new DataView[A, B](mapping)
  }

  private def swapArgs[A, B, C, D](f: (A, B) => (C, D)): (B, A) => (D, C) = {
    case (b: B, a: A) => f(a, b).swap
  }

  /** [[DataView]]s are bidirectional so need only be defined in one direction*/
  // Rob Norris (tpolecat) advises against this: https://gitter.im/scala/scala?at=6080bd5881866c680c3ac477
  //   Alternative is to require the user to provide both (.swap on DataView?) or perhaps apply method returns 2
  implicit def swapDataView[A <: Data, B <: Data](implicit d: DataView[B, A]): DataView[A, B] =
    new DataView[A, B](d.mapping.map(swapArgs(_)))

  // Built in DataViews
  //implicit val uintAsUInt = new DataView[UInt, UInt]({ case (x, y) => (x, y) }) { }

  /** All Chisel Data are viewable as their own type
    * @todo how to handle width differences?
    */
  implicit def identityView[A <: Data]: DataView[A, A] = DataView[A, A]({ case (x, y) => (x, y) })
}