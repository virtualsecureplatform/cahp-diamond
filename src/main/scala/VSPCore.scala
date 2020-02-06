import chisel3._

class MainRegisterOutPort(implicit val conf:CAHPConfig) extends Bundle{
  val x0 = Output(UInt(16.W))
  val x1 = Output(UInt(16.W))
  val x2 = Output(UInt(16.W))
  val x3 = Output(UInt(16.W))
  val x4 = Output(UInt(16.W))
  val x5 = Output(UInt(16.W))
  val x6 = Output(UInt(16.W))
  val x7 = Output(UInt(16.W))
  val x8 = Output(UInt(16.W))
  val x9 = Output(UInt(16.W))
  val x10 = Output(UInt(16.W))
  val x11 = Output(UInt(16.W))
  val x12 = Output(UInt(16.W))
  val x13 = Output(UInt(16.W))
  val x14 = Output(UInt(16.W))
  val x15 = Output(UInt(16.W))
}

class VSPCorePort(implicit val conf:CAHPConfig) extends Bundle {
  val romAddr = Output(UInt(7.W))
  val romData = Input(UInt(32.W))

  val finishFlag = Output(Bool())
  val regOut = new MainRegisterOutPort()
}

class VSPCoreNoROM extends Module{
  implicit val conf = CAHPConfig()
  conf.test = true
  val io = IO(new VSPCorePort)
  val coreUnit = Module(new CoreUnit)
  val memA = Module(new ExternalRam)
  val memB = Module(new ExternalRam)

  io.finishFlag := coreUnit.io.finishFlag
  io.regOut := coreUnit.io.regOut

  io.romAddr := coreUnit.io.romAddr
  coreUnit.io.romData := io.romData

  memA.io.address := coreUnit.io.memA.address
  memA.io.in := coreUnit.io.memA.in
  memA.io.writeEnable := coreUnit.io.memA.writeEnable
  coreUnit.io.memA.out := memA.io.out
  memB.io.address := coreUnit.io.memB.address
  memB.io.in := coreUnit.io.memB.in
  memB.io.writeEnable := coreUnit.io.memB.writeEnable
  coreUnit.io.memB.out := memB.io.out
}

class CoreMemPort(val conf:CAHPConfig) extends Bundle {
  val in = Input(UInt(8.W))
  val address = Input(UInt(8.W))
  val writeEnable = Input(Bool())
  val out = Output(UInt(8.W))

  override def cloneType: this.type = new CoreMemPort(conf).asInstanceOf[this.type]
}

class VSPCoreNoRAMROM extends Module {
  implicit val conf = CAHPConfig()
  conf.test = true
  conf.load = false
  val io = IO(new Bundle{
    val memA = Flipped(new CoreMemPort(conf))
    val memB = Flipped(new CoreMemPort(conf))
    val romAddr = Output(UInt(7.W))
    val romData = Input(UInt(32.W))

    val finishFlag = Output(Bool())
    val regOut = new MainRegisterOutPort()
  })

  val coreUnit = Module(new CoreUnit)

  io.finishFlag := coreUnit.io.finishFlag
  io.regOut := coreUnit.io.regOut
  io.romAddr := coreUnit.io.romAddr
  coreUnit.io.romData := io.romData

  io.memA.address := coreUnit.io.memA.address
  io.memA.in := coreUnit.io.memA.in
  io.memA.writeEnable := coreUnit.io.memA.writeEnable
  coreUnit.io.memA.out := io.memA.out

  io.memB.address := coreUnit.io.memB.address
  io.memB.in := coreUnit.io.memB.in
  io.memB.writeEnable := coreUnit.io.memB.writeEnable
  coreUnit.io.memB.out := io.memB.out
}
