/*
Copyright 2019 Naoki Matsumoto

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

import chisel3._

class CoreUnitPort(implicit val conf:CAHPConfig) extends Bundle {
  val romInst = Input(UInt(conf.romCacheWidth.W))
  val romAddr = Output(UInt(conf.romAddrWidth.W))
  val memA = Flipped(new MemPort)
  val memB = Flipped(new MemPort)

  val testRegx8 = if (conf.test) Output(UInt(16.W)) else Output(UInt(0.W))
  val testFinish = if (conf.test) Output(Bool()) else Output(UInt(0.W))
}

class CoreUnit(implicit val conf: CAHPConfig) extends Module {
  val io = IO(new CoreUnitPort)


  val st = Module(new StateMachine)
  val ifUnit = Module(new IfUnit)
  val idwbUnit = Module(new IdWbUnit)
  val exUnit = Module(new ExUnit)
  val memUnit = Module(new MemUnit)


  ifUnit.io.enable := st.io.clockIF
  ifUnit.io.in.jump := false.B
  ifUnit.io.in.jumpAddress := DontCare
  ifUnit.io.in.romData := io.romInst
  io.romAddr := ifUnit.io.out.romAddress

  io.memA.address := DontCare
  io.memA.in := DontCare
  io.memA.writeEnable := DontCare
  io.memB.address := DontCare
  io.memB.in := DontCare
  io.memB.writeEnable := DontCare
  io.testFinish := DontCare
  io.testRegx8 := DontCare


  idwbUnit.io.idIn.inst := ifUnit.io.out.instOut
  idwbUnit.io.idIn.pc := ifUnit.io.out.pcAddress
  idwbUnit.io.idEnable := st.io.clockID
  idwbUnit.io.wbEnable := st.io.clockWB
  idwbUnit.io.wbIn.regWrite := DontCare
  idwbUnit.io.wbIn.regWriteEnable := DontCare
  idwbUnit.io.wbIn.regWriteData := DontCare
  //idwbUnit.io.wbEnable := st.io.clockWB
  //idwbUnit.io.pc := ifUnit.io.romAddress
  //idwbUnit.io.FLAGS := exUnit.io.out.flag

  exUnit.io.enable := st.io.clockEX
  exUnit.io.in := idwbUnit.io.exOut

  memUnit.io.enable := st.io.clockMEM
  memUnit.io.in := idwbUnit.io.memOut
  memUnit.io.in.address := exUnit.io.out.res
  memUnit.io.in.in := DontCare
  //memUnit.io.regWriteEnableIn := exUnit.io.regWriteEnableOut
  //memUnit.io.regWriteIn := exUnit.io.regWriteOut
  //io.memA.address := memUnit.io.memA.address
  //io.memA.in := memUnit.io.memA.in
  //io.memA.writeEnable := memUnit.io.memA.writeEnable
  //memUnit.io.memA.out := io.memA.out
  //io.memB.address := memUnit.io.memB.address
  //io.memB.in := memUnit.io.memB.in
  //io.memB.writeEnable := memUnit.io.memB.writeEnable
  //memUnit.io.memB.out := io.memB.out

  idwbUnit.io.wbIn := idwbUnit.io.wbOut
  idwbUnit.io.wbIn.regWriteData := memUnit.io.out.out
  //idwbUnit.io.exRegWrite := exUnit.io.regWriteOut
  //idwbUnit.io.exRegWriteEnable := exUnit.io.regWriteEnableOut
  //idwbUnit.io.exFwdData := exUnit.io.fwdData
  //idwbUnit.io.exMemRead := exUnit.io.memReadOut
  //idwbUnit.io.exMemWrite := exUnit.io.memWriteOut
  //idwbUnit.io.memRegWrite := memUnit.io.regWriteOut
  //idwbUnit.io.memRegWriteEnable := memUnit.io.regWriteEnableOut
  //idwbUnit.io.memFwdData := memUnit.io.fwdData

  //io.testRegx8 := idwbUnit.io.testRegx8
  //io.testFinish := idwbUnit.io.testFinish
}
