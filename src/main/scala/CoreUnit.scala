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
  val testClockIF = if (conf.test) Output(Bool()) else Output(UInt(0.W))
}

class CoreUnit(implicit val conf: CAHPConfig) extends Module {
  val io = IO(new CoreUnitPort)


  val st = Module(new StateMachine)
  val ifUnit = Module(new IfUnit)
  val idwbUnit = Module(new IdWbUnit)
  val exUnit = Module(new ExUnit)
  val memUnit = Module(new MemUnit)

  val rom = Module(new ExternalTestRom)
  io.romAddr := DontCare
  rom.io.romAddress := ifUnit.io.out.romAddress

  ifUnit.io.enable := st.io.clockIF&&(!idwbUnit.io.stole)
  ifUnit.io.in.jump := exUnit.io.out.jump
  ifUnit.io.in.jumpAddress := exUnit.io.out.jumpAddress
  ifUnit.io.in.romData := rom.io.romData

  io.testRegx8 := idwbUnit.io.testRegx8
  io.testFinish := DontCare
  io.testClockIF := st.io.clockIF


  idwbUnit.io.idIn.inst := ifUnit.io.out.instOut
  idwbUnit.io.idIn.pc := ifUnit.io.out.pcAddress
  idwbUnit.io.exWbIn := exUnit.io.wbOut
  idwbUnit.io.exMemIn := exUnit.io.memOut
  idwbUnit.io.memWbIn := memUnit.io.wbOut
  idwbUnit.io.flush := exUnit.io.out.jump
  idwbUnit.io.idEnable := st.io.clockID
  idwbUnit.io.wbEnable := st.io.clockWB


  exUnit.io.in     := idwbUnit.io.exOut
  exUnit.io.memIn  := idwbUnit.io.memOut
  exUnit.io.wbIn   := idwbUnit.io.wbOut
  exUnit.io.enable := st.io.clockEX
  exUnit.io.flush  := exUnit.io.out.jump

  memUnit.io.enable := st.io.clockMEM
  memUnit.io.in     := exUnit.io.memOut
  memUnit.io.wbIn   := exUnit.io.wbOut

  io.memA.address := memUnit.io.memA.address
  io.memA.in := memUnit.io.memA.in
  io.memA.writeEnable := memUnit.io.memA.writeEnable
  io.memB.address := memUnit.io.memB.address
  io.memB.in := memUnit.io.memB.in
  io.memB.writeEnable := memUnit.io.memB.writeEnable

  memUnit.io.memA.out := io.memA.out
  memUnit.io.memB.out := io.memB.out

  idwbUnit.io.wbIn := memUnit.io.wbOut
}
