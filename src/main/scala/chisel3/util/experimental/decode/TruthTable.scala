// SPDX-License-Identifier: Apache-2.0

package chisel3.util.experimental.decode

import chisel3.util.BitPat

case class TruthTable(table: Map[BitPat, BitPat], default: BitPat)