import chisel3._

class VSPCorePort(implicit val conf:CAHPConfig) extends Bundle {
  val romAddr = Output(UInt(7.W))
  val romData = Input(UInt(32.W))

  val finishFlag = Output(Bool())
  val testRegx8 = Output(UInt(16.W))
}

class VSPCore() extends Module{
  implicit val conf = CAHPConfig()
  conf.test = true
  val io = IO(new VSPCorePort)
  val coreUnit = Module(new CoreUnit)
  val memA = Module(new ExternalRam)
  val memB = Module(new ExternalRam)

  io.testRegx8 := coreUnit.io.testRegx8
  io.finishFlag := coreUnit.io.finishFlag

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
