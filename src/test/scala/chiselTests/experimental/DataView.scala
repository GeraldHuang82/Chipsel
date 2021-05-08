// See LICENSE for license details.

package chiselTests.experimental

import chiselTests.ChiselFlatSpec
import chisel3._
import chisel3.experimental.dataview._
import chisel3.stage.ChiselStage
import chisel3.testers.BasicTester
import chisel3.util.Counter

object SimpleBundleDataView {
  class BundleA extends Bundle {
    val foo = UInt(8.W)
  }
  class BundleB extends Bundle {
    val bar = UInt(8.W)
  }
  implicit val view = DataView[BundleA, BundleB](_.foo -> _.bar)
}

class DataViewSpec extends ChiselFlatSpec {

  behavior of "DataView"

  it should "support simple Bundle viewing" in {
    import SimpleBundleDataView._
    class MyModule extends Module {
      val in = IO(Input(new BundleA))
      val out = IO(Output(new BundleB))
      out := in.viewAs(new BundleB)
    }
    val chirrtl = ChiselStage.emitChirrtl(new MyModule)
    chirrtl should include("out.bar <= in.foo")
  }

  it should "be a bidirectional mapping" in {
    import SimpleBundleDataView._
    class MyModule extends Module {
      val in = IO(Input(new BundleA))
      val out = IO(Output(new BundleB))
      out.viewAs(new BundleA) := in
    }
    val chirrtl = ChiselStage.emitChirrtl(new MyModule)
    chirrtl should include("out.bar <= in.foo")
  }

  it should "handle viewing UInts as UInts" in {
    class MyModule extends Module {
      val in = IO(Input(UInt(8.W)))
      val foo = IO(Output(UInt(8.W)))
      val bar = IO(Output(UInt(8.W)))
      foo := in.viewAs(UInt(8.W))
      bar.viewAs(UInt(8.W)) := in
    }
    val chirrtl = ChiselStage.emitChirrtl(new MyModule)
    chirrtl should include("foo <= in")
    chirrtl should include("bar <= in")
  }

  it should "handle viewing Bundles as their same concrete type" in {
    class MyBundle extends Bundle {
      val foo = UInt(8.W)
    }
    class MyModule extends Module {
      val in = IO(Input(new MyBundle))
      val fizz = IO(Output(new MyBundle))
      val buzz = IO(Output(new MyBundle))
      fizz := in.viewAs(new MyBundle)
      buzz.viewAs(new MyBundle) := in
    }
    val chirrtl = ChiselStage.emitChirrtl(new MyModule)
    chirrtl should include("fizz.foo <= in.foo")
    chirrtl should include("buzz.foo <= in.foo")
  }

}