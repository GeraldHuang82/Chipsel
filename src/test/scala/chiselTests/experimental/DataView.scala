// See LICENSE for license details.

package chiselTests.experimental

import chiselTests.ChiselFlatSpec
import chisel3._
import chisel3.experimental.dataview._
import chisel3.stage.ChiselStage
import chisel3.util.{Decoupled, DecoupledIO}

object SimpleBundleDataView {
  class BundleA extends Bundle {
    val foo = UInt(8.W)
  }
  class BundleB extends Bundle {
    val bar = UInt(8.W)
  }
  implicit val view = DataView[BundleA, BundleB](_.foo -> _.bar)
}

object VecBundleDataView {
  class MyBundle extends Bundle {
    val foo = UInt(8.W)
    val bar = UInt(8.W)
  }
  implicit val view = DataView[MyBundle, Vec[UInt]](_.foo -> _(1), _.bar -> _(0))
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

  it should "handle viewing Vecs as their same concrete type" in {
    class MyModule extends Module {
      val in = IO(Input(Vec(1, UInt(8.W))))
      val fizz = IO(Output(Vec(1, UInt(8.W))))
      val buzz = IO(Output(Vec(1, UInt(8.W))))
      fizz := in.viewAs(Vec(1, UInt(8.W)))
      buzz.viewAs(Vec(1, UInt(8.W))) := in
    }
    val chirrtl = ChiselStage.emitChirrtl(new MyModule)
    chirrtl should include("fizz[0] <= in[0]")
    chirrtl should include("buzz[0] <= in[0]")
  }

  it should "handle viewing Vecs as Bundles and vice versa" in {
    import VecBundleDataView._
    class MyModule extends Module {
      val in = IO(Input(new MyBundle))
      val out = IO(Output(Vec(2, UInt(8.W))))
      val out2 = IO(Output(Vec(2, UInt(8.W))))
      out := in.viewAs(Vec(2, UInt(8.W)))
      out2.viewAs(new MyBundle) := in
    }
    val chirrtl = ChiselStage.emitChirrtl(new MyModule)
    chirrtl should include("out[0] <= in.bar")
    chirrtl should include("out[1] <= in.foo")
    chirrtl should include("out2[0] <= in.bar")
    chirrtl should include("out2[1] <= in.foo")
  }

  it should "work with bidirectional connections for nested types" in {
    class FizzBuzz extends Bundle {
      val fizz = UInt(8.W)
      val buzz = UInt(8.W)
    }
    class FlatDecoupled extends Bundle {
      val valid = Output(Bool())
      val ready = Input(Bool())
      val fizz = Output(UInt(8.W))
      val buzz = Output(UInt(8.W))
    }
    implicit val view = DataView[FlatDecoupled, DecoupledIO[FizzBuzz]](
      _.valid -> _.valid,
      _.ready -> _.ready,
      _.fizz -> _.bits.fizz,
      _.buzz -> _.bits.buzz
    )
    class MyModule extends Module {
      val enq = IO(Flipped(Decoupled(new FizzBuzz)))
      val deq = IO(new FlatDecoupled)
      val deq2 = IO(new FlatDecoupled)
      deq <> enq.viewAs(new FlatDecoupled)
      deq2.viewAs(Decoupled(new FizzBuzz)) <> enq
    }
    val chirrtl = ChiselStage.emitChirrtl(new MyModule)
    chirrtl should include("deq.valid <= enq.valid")
    chirrtl should include("enq.ready <= deq.ready")
    chirrtl should include("deq.fizz <= enq.bits.fizz")
    chirrtl should include("deq.buzz <= enq.bits.buzz")
    chirrtl should include("deq2.valid <= enq.valid")
    chirrtl should include("enq.ready <= deq2.ready")
    chirrtl should include("deq2.fizz <= enq.bits.fizz")
    chirrtl should include("deq2.buzz <= enq.bits.buzz")
  }

}