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

class ExUnitPort(implicit val conf:CAHPConfig) extends Bundle {
  val in = new ExUnitIn
  val enable = Input(Bool())

  val out = new ExUnitOut
}
class ExUnitIn extends Bundle {
  val inA = Input(UInt(16.W))
  val inB = Input(UInt(16.W))
  val opcode = Input(UInt(4.W))

  val pcOpcode = Input(UInt(3.W))
  val pc = Input(UInt(16.W))
  val pcImm = Input(UInt(16.W))
  val pcAdd = Input(Bool())
}

class ExUnitOut(implicit val conf:CAHPConfig) extends Bundle {
  val res = Output(UInt(16.W))
  val jumpAddress = Output(UInt(conf.romAddrWidth.W))
  val jump = Output(Bool())
}

class ExUnit(implicit val conf:CAHPConfig) extends Module {
  val io = IO(new ExUnitPort)
  val pExReg = RegInit(0.U.asTypeOf(new ExUnitIn))

  def check_overflow(s1: UInt, s2: UInt, r: UInt) = {
    val s1_sign = Wire(UInt(1.W))
    val s2_sign = Wire(UInt(1.W))
    val res_sign = Wire(UInt(1.W))
    val res = Wire(Bool())
    s1_sign := s1(15)
    s2_sign := s2(15)
    res_sign := r(15)
    when(((s1_sign ^ s2_sign) === 0.U) && ((s2_sign ^ res_sign) === 1.U)) {
      res := true.B
    }.otherwise {
      res := false.B
    }
    res
  }

  val flagCarry = Wire(Bool())
  val flagOverflow = Wire(Bool())
  val flagSign = Wire(Bool())
  val flagZero = Wire(Bool())
  val resCarry = Wire(UInt(17.W))
  val inB_sub = Wire(UInt(16.W))
  inB_sub := (~pExReg.inB).asUInt()+1.U

  flagCarry := DontCare
  flagOverflow := DontCare
  flagSign := DontCare
  flagZero := DontCare
  resCarry := DontCare

  when(io.enable) {
    pExReg := io.in
  }

  when(pExReg.opcode === ALUOpcode.ADD) {
    io.out.res := pExReg.inA + pExReg.inB
  }.elsewhen(pExReg.opcode === ALUOpcode.SUB) {
    resCarry := pExReg.inA +& inB_sub
    io.out.res := resCarry(15, 0)
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
  }.elsewhen(pExReg.opcode === ALUOpcode.MOV) {
    io.out.res := pExReg.inB
  }.otherwise {
    io.out.res := DontCare
  }

  when(io.in.pcAdd) {
    io.out.jumpAddress := io.in.pc + io.in.pcImm
  }.otherwise{
    io.out.jumpAddress := io.in.pcImm
  }

  flagCarry := ~resCarry(16)
  flagSign := io.out.res(15)
  flagZero := (io.out.res === 0.U(16.W))
  flagOverflow := check_overflow(pExReg.inA, inB_sub, io.out.res)
  io.out.jump := false.B
  when(io.in.pcOpcode === 1.U){
    io.out.jump := flagZero
  }.elsewhen(io.in.pcOpcode === 2.U){
    io.out.jump := flagCarry
  }.elsewhen(io.in.pcOpcode === 3.U){
    io.out.jump := flagCarry||flagZero
  }.elsewhen(io.in.pcOpcode === 4.U){
    io.out.jump := true.B
  }.elsewhen(io.in.pcOpcode === 5.U){
    io.out.jump := !flagZero
  }.elsewhen(io.in.pcOpcode === 6.U){
    io.out.jump := (flagSign != flagOverflow)
  }.elsewhen(io.in.pcOpcode === 7.U){
    io.out.jump := (flagSign != flagOverflow)||flagZero
  }
  //printf("[EX] FLAGS Carry:%d Sign:%d Zero:%d OverFlow:%d\n", flagCarry, flagSign, flagZero, flagOverflow)

  when(conf.debugEx.B) {
    printf("[EX] opcode:0x%x\n", pExReg.opcode)
    printf("[EX] inA:0x%x\n", pExReg.inA)
    printf("[EX] inB:0x%x\n", pExReg.inB)
    printf("[EX] Res:0x%x\n", io.out.res)
  }
}
object ALUOpcode {
  def ADD = BitPat("b0000")
  def SUB = BitPat("b0001")
  def AND = BitPat("b0010")
  def XOR = BitPat("b0011")
  def OR  = BitPat("b0100")
  def LSL = BitPat("b0101")
  def LSR = BitPat("b0110")
  def ASR = BitPat("b0111")
  def MOV = BitPat("b1000")
}
