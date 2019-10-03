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

class ExUnitInput extends Bundle {
  val opcode = Input(UInt(3.W))
  val inA = Input(UInt(16.W))
  val inB = Input(UInt(16.W))
}

class ExUnitOutput extends Bundle {
  val res = Output(UInt(16.W))
  val flag = Output(UInt(4.W))
}

class ExUnitIO extends Bundle {
  val in = new ExUnitInput
  val out = new ExUnitOutput
}

class ExIO extends Bundle {
  val in = new ExUnitInput
  val shifterSig = Input(Bool())
  val Enable = Input(Bool())
  val memWriteDataIn = Input(UInt(16.W))
  val memByteEnableIn = Input(Bool())
  val memSignExtIn = Input(Bool())
  val memReadIn = Input(Bool())
  val memWriteIn = Input(Bool())
  val regWriteEnableIn = Input(Bool())
  val regWriteIn = Input(UInt(4.W))

  val out = new ExUnitOutput
  val memWriteDataOut = Output(UInt(16.W))
  val memByteEnableOut = Output(Bool())
  val memSignExtOut = Output(Bool())
  val memReadOut = Output(Bool())
  val memWriteOut = Output(Bool())
  val regWriteEnableOut = Output(Bool())
  val regWriteOut = Output(UInt(4.W))

  val fwdData = Output(UInt(16.W))
}

class ExReg extends Bundle {
  val opcode = UInt(4.W)
  val inA = UInt(16.W)
  val inB = UInt(16.W)
  val shifterSig = Bool()

  val memWriteData = UInt(16.W)
  val memByteEnable = Bool()
  val memSignExt = Bool()
  val memRead = Bool()
  val memWrite = Bool()

  val regWriteEnable = Bool()
  val regWrite = UInt(4.W)
}

class ExUnit(implicit val conf:RV16KConfig) extends Module {
  val io = IO(new ExIO)
  val alu = Module(new ALU)
  val shifter = Module(new Shifter)
  val pReg = RegInit(0.U.asTypeOf(new ExReg))

  when(io.Enable) {
    pReg.opcode := io.in.opcode
    pReg.inA := io.in.inA
    pReg.inB := io.in.inB
    pReg.shifterSig := io.shifterSig
    pReg.memWriteData := io.memWriteDataIn
    pReg.memByteEnable := io.memByteEnableIn
    pReg.memSignExt := io.memSignExtIn
    pReg.memRead := io.memReadIn
    pReg.memWrite := io.memWriteIn
    pReg.regWriteEnable := io.regWriteEnableIn
    pReg.regWrite := io.regWriteIn
  }

  alu.io.in.opcode := pReg.opcode
  alu.io.in.inA := pReg.inA
  alu.io.in.inB := pReg.inB
  shifter.io.in.opcode := pReg.opcode
  shifter.io.in.inA := pReg.inA
  shifter.io.in.inB := pReg.inB

  when(pReg.shifterSig) {
    io.out := shifter.io.out
  }.otherwise {
    io.out := alu.io.out
  }
  io.memWriteDataOut := pReg.memWriteData
  io.memByteEnableOut := pReg.memByteEnable
  io.memSignExtOut := pReg.memSignExt
  io.memReadOut := pReg.memRead
  io.memWriteOut := pReg.memWrite
  io.regWriteEnableOut := pReg.regWriteEnable
  io.regWriteOut := pReg.regWrite
  io.fwdData := io.out.res

  when(conf.debugEx.B) {
    printf("[EX] opcode:0x%x\n", pReg.opcode)
    printf("[EX] inA:0x%x\n", pReg.inA)
    printf("[EX] inB:0x%x\n", pReg.inB)
    printf("[EX] Res:0x%x\n", io.out.res)
    printf("[EX] FLAGS:0x%x\n", io.out.flag)
  }
}

class ALU extends Module {
  val io = IO(new ExUnitIO)
  val flagCarry = Wire(UInt(1.W))
  val resCarry = Wire(UInt(17.W))
  val flagOverflow = Wire(UInt(1.W))

  resCarry := DontCare
  flagCarry := 0.U(1.W)
  flagOverflow := 0.U(1.W)
  when(io.in.opcode === ALUOpcode.MOV) {
    io.out.res := io.in.inB
  }.elsewhen(io.in.opcode === ALUOpcode.ADD) {
    resCarry := io.in.inA +& io.in.inB
    io.out.res := resCarry(15, 0)
    flagCarry := ~resCarry(16)
    flagOverflow := FLAGS.check_overflow(io.in.inA, io.in.inB, io.out.res)
  }.elsewhen(io.in.opcode === ALUOpcode.SUB) {
    val inB_sub = Wire(UInt(16.W))
    inB_sub := (~io.in.inB).asUInt()+1.U
    resCarry := io.in.inA +& inB_sub
    io.out.res := resCarry(15, 0)
    flagCarry := ~resCarry(16)
    flagOverflow := FLAGS.check_overflow(io.in.inA, inB_sub, io.out.res)
  }.elsewhen(io.in.opcode === ALUOpcode.AND) {
    io.out.res := io.in.inA & io.in.inB
  }.elsewhen(io.in.opcode === ALUOpcode.OR) {
    io.out.res := io.in.inA | io.in.inB
  }.elsewhen(io.in.opcode === ALUOpcode.XOR) {
    io.out.res := io.in.inA ^ io.in.inB
  }.otherwise {
    io.out.res := DontCare
  }
  io.out.flag := Cat(FLAGS.check_sign(io.out.res), FLAGS.check_zero(io.out.res), flagCarry, flagOverflow)
}

class Shifter extends Module {
  val io = IO(new ExUnitIO)
  when(io.in.opcode === ShifterOpcode.LSL) {
    io.out.res := (io.in.inA << io.in.inB).asUInt()
  }.elsewhen(io.in.opcode === ShifterOpcode.LSR) {
    io.out.res := (io.in.inA >> io.in.inB).asUInt()
  }.elsewhen(io.in.opcode === ShifterOpcode.ASR) {
    io.out.res := ((io.in.inA.asSInt()) >> io.in.inB).asUInt()
  }.otherwise {
    io.out.res := DontCare
  }
  io.out.flag := Cat(FLAGS.check_sign(io.out.res), FLAGS.check_zero(io.out.res), 0.U(2.W))
}

object ALUOpcode {
  def MOV = BitPat("b000")
  def ADD = BitPat("b010")
  def SUB = BitPat("b011")
  def AND = BitPat("b100")
  def OR = BitPat("b101")
  def XOR = BitPat("b110")
}

object ShifterOpcode {
  def LSL = BitPat("b001")
  def LSR = BitPat("b010")
  def ASR = BitPat("b101")
}

object FLAGS {
  def check_sign(t: UInt): UInt = t(15)

  def check_zero(t: UInt): UInt = {
    val res = Wire(UInt(1.W))
    when(t === 0.U(16.W)) {
      res := 1.U
    }.otherwise {
      res := 0.U
    }
    res
  }

  def check_overflow(s1: UInt, s2: UInt, r: UInt) = {
    val s1_sign = Wire(UInt(1.W))
    val s2_sign = Wire(UInt(1.W))
    val res_sign = Wire(UInt(1.W))
    val res = Wire(UInt(1.W))
    s1_sign := s1(15)
    s2_sign := s2(15)
    res_sign := r(15)
    when(((s1_sign ^ s2_sign) === 0.U) && ((s2_sign ^ res_sign) === 1.U)) {
      res := 1.U(1.W)
    }.otherwise {
      res := 0.U(1.W)
    }
    res
  }
}
