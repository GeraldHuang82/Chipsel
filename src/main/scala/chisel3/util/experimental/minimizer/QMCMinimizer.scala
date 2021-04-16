package chisel3.util.experimental.minimizer

import chisel3.util.BitPat

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import Minimizer._

object QMCMinimizer {
  /** @return A instance of [[QMCMinimizer]] */
  def apply(): QMCMinimizer = new QMCMinimizer()

  /** Calculate essential prime implicants based on previously calculated prime implicants and all implicants.
    *
    * @param primes    Prime implicants
    * @param minterms All implicants
    * @return (a, b, c)
    *         a: essential prime implicants
    *         b: nonessential prime implicants
    *         c: implicants that are not cover by any of the essential prime implicants
    */
  def getEssentialPrimeImplicants(primes: Seq[Implicant], minterms: Seq[Implicant]): (Seq[Implicant], Seq[Implicant], Seq[Implicant]) = {
    // primeCovers(i): implicants that `prime(i)` covers
    val primeCovers = primes.map(p => minterms.filter(p.covers))
    // eliminate prime implicants that can be covered by other prime implicants
    for (((icover, pi), i) <- (primeCovers zip primes).zipWithIndex) {
      for (((jcover, pj), _) <- (primeCovers zip primes).zipWithIndex.drop(i + 1)) {
        // we prefer prime implicants with wider implicants coverage
        if (icover.size > jcover.size && jcover.forall(pi.covers)) {
          // calculate essential prime implicants with `pj` eliminated from prime implicants table
          return getEssentialPrimeImplicants(primes.filter(_ != pj), minterms)
        }
      }
    }

    // implicants that only one prime implicant covers
    val essentiallyCovered = minterms.filter(t => primes.count(_.covers(t)) == 1)
    // essential prime implicants, prime implicants that covers only one implicant
    val essential = primes.filter(p => essentiallyCovered.exists(p.covers))
    // {nonessential} = {prime implicants} - {essential prime implicants}
    val nonessential = primes.filterNot(essential contains _)
    // implicants that no essential prime implicants covers
    val uncovered = minterms.filterNot(t => essential.exists(_.covers(t)))
    if (essential.isEmpty || uncovered.isEmpty)
      (essential, nonessential, uncovered)
    else {
      // now there are implicants (`uncovered`) that are covered by multiple nonessential prime implicants (`nonessential`)
      // need to reduce prime implicants
      val (a, b, c) = getEssentialPrimeImplicants(nonessential, uncovered)
      (essential ++ a, b, c)
    }
  }

  /** Use [[https://en.wikipedia.org/wiki/Petrick%27s_method]] to select a [[Seq]] of nonessential prime implicants
    * that covers all implicants that are not covered by essential prime implicants.
    *
    * @param implicants Nonessential prime implicants
    * @param minterms   Implicants that are not covered by essential prime implicants
    * @return Selected nonessential prime implicants
    */
  def getCover(implicants: Seq[Implicant], minterms: Seq[Implicant]): Seq[Implicant] = {
    /** Calculate the implementation cost (using comparators) of a list of implicants, more don't cares is cheaper
      *
      * @param cover Implicant list
      * @return How many comparators need to implement this list of implicants
      */
    def getCost(cover: Seq[Implicant]): Int = cover.map(_.bp.mask.bitCount).sum

    /** Determine if one combination of prime implicants is cheaper when implementing as comparators.
      * Shorter term list is cheaper, term list with more don't cares is cheaper (less comparators)
      *
      * @param a    Operand a
      * @param b    Operand b
      * @return `a` < `b`
      */
    def cheaper(a: Seq[Implicant], b: Seq[Implicant]): Boolean = {
      val ca = getCost(a)
      val cb = getCost(b)

      /** If `a` < `b`
        *
        * Like comparing the dictionary order of two strings.
        * Define `a` < `b` if both `a` and `b` are empty.
        *
        * @param a Operand a
        * @param b Operand b
        * @return `a` < `b`
        */
      @tailrec
      def listLess(a: Seq[Implicant], b: Seq[Implicant]): Boolean = b.nonEmpty && (a.isEmpty || a.head < b.head || a.head == b.head && listLess(a.tail, b.tail))

      ca < cb || ca == cb && listLess(a.sortWith(_ < _), b.sortWith(_ < _))
    }

    // if there are no implicant that is not covered by essential prime implicants, which means all implicants are
    // covered by essential prime implicants, there is no need to apply Petrick's method
    if (minterms.nonEmpty) {
      // cover(i): nonessential prime implicants that covers `minterms(i)`
      val cover = minterms.map(m => implicants.filter(_.covers(m)))
      // apply [[Petrick's method https://en.wikipedia.org/wiki/Petrick%27s_method]]
      val all = cover.tail.foldLeft(cover.head.map(Set(_)))((c0, c1) => c0.flatMap(a => c1.map(a + _)))
      all.map(_.toList).reduceLeft((a, b) => if (cheaper(a, b)) a else b)
    } else
      Seq[Implicant]()
  }
}

/** The [[https://en.wikipedia.org/wiki/Quine-McCluskey_algorithm]] decoder implementation. */
class QMCMinimizer extends Minimizer {
  def minimize(default: BitPat, table: Seq[(BitPat, BitPat)]): Seq[(BitPat, BitPat)] = {

    require(table.nonEmpty, "Truth table must not be empty")

    // extract decode table to inputs and outputs
    val (inputs, outputs) = table.unzip

    require(outputs.map(_.getWidth == default.getWidth).reduce(_ && _), "All output BitPats and default BitPat must have the same length")
    require(if (inputs.length > 1) inputs.tail.map(_.width == inputs.head.width).reduce(_ && _) else true, "All input BitPats must have the same length")

    // make sure no two inputs specified in the truth table intersect
    for (t <- inputs.tails; if t.nonEmpty)
      for (u <- t.tail)
        require(!t.head.intersects(u), "truth table entries " + t.head + " and " + u + " overlap")

    // number of inputs
    val n = inputs.head.width
    // number of outputs
    val m = outputs.head.getWidth

    // for all outputs
    (0 until m).flatMap(i => {
      val outputBp = BitPat("b" + "?" * (m - i - 1) + "1" + "?" * i)

      // Minterms, implicants that makes the output to be 1
      val mint: Seq[Implicant] = table.filter { case (_, t) => t.mask.testBit(i) && t.value.testBit(i) }.map(_._1).map(toImplicant)
      // Maxterms, implicants that makes the output to be 0
      val maxt: Seq[Implicant] = table.filter { case (_, t) => t.mask.testBit(i) && !t.value.testBit(i) }.map(_._1).map(toImplicant)
      // Don't cares, implicants that can produce either 0 or 1 as output
      val dc: Seq[Implicant] = table.filter { case (_, t) => !t.mask.testBit(i) }.map(_._1).map(toImplicant)

      val (implicants, defaultToDc) = default match {
        case x if x.mask.testBit(i) && !x.value.testBit(i) => // default to 0
          (mint ++ dc, false)
        case x if x.mask.testBit(i) && x.value.testBit(i) => // default to 1
          (maxt ++ dc, false)
        case x if !x.mask.testBit(i) => // default to ?
          (mint, true)
      }

      if (!defaultToDc && dc.isEmpty) {
        // As an elaboration performance optimization, don't be too clever if
        // there are no don't-cares; synthesis can figure it out.
        return implicants.map(a => (a.bp, outputBp))
      }

      implicants.foreach(_.isPrime = true)
      val cols = (0 to n).reverse.map(b => implicants.filter(b == _.bp.mask.bitCount))
      val mergeTable = cols.map(
        c => (0 to n).map(
          b => collection.mutable.Set(c.filter(b == _.bp.value.bitCount):_*)
        )
      )

      var primeImplicants = List[Implicant]()
      for (i <- 0 to n) {
        for (j <- 0 until n - i) {
          mergeTable(i)(j).foreach(a => mergeTable(i + 1)(j) ++= mergeTable(i)(j + 1).filter(_ similar a).map(_ merge a))
        }
        if (defaultToDc) {
          for (j <- 0 until n - i) {
            for (a <- mergeTable(i)(j).filter(_.isPrime)) {
              val dc = a.getImplicitDC(maxt, above = true)
              if (dc.isDefined)
                mergeTable(i + 1)(j) += dc.get merge a
            }
            for (a <- mergeTable(i)(j + 1).filter(_.isPrime)) {
              val dc = a.getImplicitDC(maxt, above = false)
              if (dc.isDefined)
                mergeTable(i + 1)(j) += a merge dc.get
            }
          }
        }
        for (r <- mergeTable(i))
          for (p <- r; if p.isPrime)
            primeImplicants = p :: primeImplicants
      }

      primeImplicants = primeImplicants.sortWith(_ < _)

      val (essentialPrimeImplicants, nonessentialPrimeImplicants, uncoveredImplicants) =
        QMCMinimizer.getEssentialPrimeImplicants(primeImplicants, implicants)

      (essentialPrimeImplicants ++ QMCMinimizer.getCover(nonessentialPrimeImplicants, uncoveredImplicants)).map(a => (a.bp, outputBp))
    })
  }
}