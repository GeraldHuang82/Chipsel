package chisel3.util.experimental.minimizer

import chisel3.util.BitPat

object Minimizer {
  implicit def toImplicant(x: BitPat): Implicant = new Implicant(x)

  private[minimizer] class Implicant(val bp: BitPat) {
    var isPrime: Boolean = true

    def width = bp.getWidth

    override def equals(that: Any): Boolean = that match {
      case x: Implicant => bp.value == x.bp.value && bp.mask == x.bp.mask
      case _ => false
    }

    override def hashCode = bp.value.toInt

    /** Check whether two implicants have the same value on all of the cared bits (intersection).
      *
      * {{{
      * value ^^ x.value                                       // bits that are different
      * (bits that are different) & x.mask                     // bits that are different and `this` care
      * (bits that are different and `this` care) & y.mask     // bits that are different and `both` care
      * (bits that are different and both care) == 0           // no (bits that are different and we both care) exists
      * no (bits that are different and we both care) exists   // all cared bits are the same, two terms intersect
      * }}}
      *
      * @param y Implicant to be checked with
      * @return Whether two implicants intersect
      */
    def intersects(y: Implicant): Boolean = ((bp.value ^ y.bp.value) & bp.mask & y.bp.mask) == 0

    /** Check whether two implicants are similar.
      * Two implicants are "similar" when they satisfy all the following rules:
      *   1. have the same mask ('?'s are at the same positions)
      *   1. values only differ by one bit
      *   1. the bit at the differed position of this term is '1' (that of the other term is '0')
      *
      * @example this = 11?0, x = 10?0 -> similar
      * @example this = 11??, x = 10?0 -> not similar, violated rule 1
      * @example this = 11?1, x = 10?0 -> not similar, violated rule 2
      * @example this = 10?0, x = 11?0 -> not similar, violated rule 3
      * @param y Implicant to be checked with
      * @return Whether this term is similar to the other
      */
    def similar(y: Implicant): Boolean = {
      val diff = bp.value - y.bp.value
      bp.mask == y.bp.mask && bp.value > y.bp.value && (diff & diff - 1) == 0
    }

    /** Merge two similar implicants
      * Rule of merging: '0' and '1' merge to '?'
      *
      * @todo return Option[Implicant], use flatMap.
      * @param y Term to be merged with
      * @return A new term representing the merge result
      */
    def merge(y: Implicant): Implicant = {
      require(similar(y), s"merge is only reasonable when $this similar $y")

      // if two term can be merged, then they both are not prime implicants.
      isPrime = false
      y.isPrime = false
      val bit = bp.value - y.bp.value
      new BitPat(bp.value &~ bit, bp.mask &~ bit, width)
    }

    /** Check all bits in `x` cover the correspond position in `y`.
      *
      * Rule to define coverage relationship among `0`, `1` and `?`:
      *   1. '?' covers '0' and '1', '0' covers '0', '1' covers '1'
      *   1. '1' doesn't cover '?', '1' doesn't cover '0'
      *   1. '0' doesn't cover '?', '0' doesn't cover '1'
      *
      * For all bits that `x` don't care, `y` can be `0`, `1`, `?`
      * For all bits that `x` care, `y` must be the same value and not masked.
      * {{{
      *    (~x.mask & -1) | ((x.mask) & ((x.value xnor y.value) & y.mask)) = -1
      * -> ~x.mask | ((x.mask) & ((x.value xnor y.value) & y.mask)) = -1
      * -> ~x.mask | ((x.value xnor y.value) & y.mask) = -1
      * -> x.mask & ~((x.value xnor y.value) & y.mask) = 0
      * -> x.mask & (~(x.value xnor y.value) | ~y.mask) = 0
      * -> x.mask & ((x.value ^ y.value) | ~y.mask) = 0
      * -> ((x.value ^ y.value) & x.mask | ~y.mask & x.mask) = 0
      * }}}
      *
      * @param y to check is covered by `x` or not.
      * @return Whether `x` covers `y`
      */
    def covers(y: Implicant): Boolean = ((bp.value ^ y.bp.value) & bp.mask | ~y.bp.mask & bp.mask) == 0

    /** Search for implicit don't cares of `term`. The new implicant must NOT intersect with any of the implicants from `maxterm`.
      *
      * @param maxterms The forbidden list of searching
      * @param above    Are we searching for implicants with one more `1` in value than `this`? (or search for implicants with one less `1`)
      * @return The implicants that we found or `null`
      */
    def getImplicitDC(maxterms: Seq[Implicant], above: Boolean): Option[Implicant] = {
      // foreach input outputs in implicant `term`
      for (i <- 0 until width) {
        var t: Option[Implicant] = None
        if (above && (bp.mask.testBit(i) && !bp.value.testBit(i))) // this bit is `0`
          t = Some(new BitPat(bp.value.setBit(i), bp.mask, width)) // generate a new implicant with i-th position being `1` and others the same as `this`
        else if (!above && (bp.mask.testBit(i) && bp.value.testBit(i))) // this bit is `1`
          t = Some(new BitPat(bp.value.clearBit(i), bp.mask, width)) // generate a new implicant with i-th position being `0` and others the same as `this`
        if (t.isDefined && !maxterms.exists(_.intersects(t.get))) // make sure we are not using one implicant from the forbidden list
          return t
      }
      None
    }

    override def toString = (if (!isPrime) "Non" else "") + "Prime" + bp.toString.replace("BitPat", "Implicant")
  }

  /**
    * If two terms have different value, then their order is determined by the value, or by the mask.
    */
  private[minimizer] implicit def ordering: Ordering[Implicant] = new Ordering[Implicant] {
    override def compare(x: Implicant, y: Implicant): Int =
      if (x.bp.value < y.bp.value || x.bp.value == y.bp.value && x.bp.mask > y.bp.mask) -1 else 1
  }
}

abstract class Minimizer {
  /** Minimize a multi-input multi-output logic function given by the truth table `table`, with function output values
    * on unspecified inputs treated as `default`, and return a minimized PLA-like representation of the function.
    *
    * Each bit of `table[]._1` encodes one 1-bit input variable of the logic function, and each bit of `default` and
    * `table[]._2` represents one 1-bit output value of the function.
    *
    * @param default  Default output values, can have don't cares
    * @param table    Truth table, can have don't cares in both inputs and outputs, specified as [(inputs, outputs), ...]
    * @return         Minimized truth table, [(inputs, outputs), ...]
    *
    * @example {{{
    *          minimize(BitPat("b?"), Seq(
    *              (BitPat("b000"), BitPat("b0")),
    *              // (BitPat("b001"), BitPat("b?")),  // same as default, can be omitted
    *              // (BitPat("b010"), BitPat("b?")),  // same as default, can be omitted
    *              (BitPat("b011"), BitPat("b0")),
    *              (BitPat("b100"), BitPat("b1")),
    *              (BitPat("b101"), BitPat("b1")),
    *              (BitPat("b110"), BitPat("b0")),
    *              (BitPat("b111"), BitPat("b1")),
    *          ))
    * }}}
    */
  def minimize(default: BitPat, table: Seq[(BitPat, BitPat)]): Seq[(BitPat, BitPat)]

}