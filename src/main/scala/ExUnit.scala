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
import chisel3.util.{BitPat, Cat}

class ExUnitPort extends Bundle {
  val in = new ExUnitIn
  val enable = Input(Bool())

  val out = new ExUnitOut
}
class ExUnitIn extends Bundle {
  val inA = Input(UInt(16.W))
  val inB = Input(UInt(16.W))
  val opcode = Input(UInt(3.W))
}

class ExUnitOut extends Bundle {
  val res = Output(UInt(16.W))
}

class ExUnit(implicit val conf:CAHPConfig) extends Module {
  val io = IO(new ExUnitPort)
  val pExReg = RegInit(0.U.asTypeOf(new ExUnitIn))

  when(io.enable) {
    pExReg := io.in
  }

  when(pExReg.opcode === ALUOpcode.ADD) {
    io.out.res := pExReg.inA + pExReg.inB
  }.elsewhen(pExReg.opcode === ALUOpcode.SUB) {
    io.out.res := pExReg.inA - pExReg.inB
  }.elsewhen(pExReg.opcode === ALUOpcode.AND) {
    io.out.res := pExReg.inA & pExReg.inB
  }.elsewhen(pExReg.opcode === ALUOpcode.OR) {
    io.out.res := pExReg.inA | pExReg.inB
  }.elsewhen(pExReg.opcode === ALUOpcode.XOR) {
    io.out.res := pExReg.inA ^ pExReg.inB
  }.elsewhen(pExReg.opcode === ALUOpcode.LSL) {
    io.out.res := (pExReg.inA << pExReg.inB).asUInt()
  }.elsewhen(pExReg.opcode === ALUOpcode.LSR) {
    io.out.res := (pExReg.inA >> pExReg.inB).asUInt()
  }.elsewhen(pExReg.opcode === ALUOpcode.ASR) {
    io.out.res := (pExReg.inA.asSInt() >> pExReg.inB).asUInt()
  }.otherwise {
    io.out.res := DontCare
  }
  when(conf.debugEx.B) {
    printf("[EX] opcode:0x%x\n", pExReg.opcode)
    printf("[EX] inA:0x%x\n", pExReg.inA)
    printf("[EX] inB:0x%x\n", pExReg.inB)
    printf("[EX] Res:0x%x\n", io.out.res)
  }
}
object ALUOpcode {
  def ADD = BitPat("b000")
  def SUB = BitPat("b001")
  def AND = BitPat("b010")
  def XOR = BitPat("b011")
  def OR  = BitPat("b100")
  def LSL = BitPat("b101")
  def LSR = BitPat("b110")
  def ASR = BitPat("b111")
}
