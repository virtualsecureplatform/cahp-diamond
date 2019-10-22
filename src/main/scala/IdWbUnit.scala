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
import chisel3.util.{Cat, Fill}

class IdUnitIn(implicit val conf:CAHPConfig) extends Bundle {
  val inst = Input(UInt(24.W))
  val pc = Input(UInt(9.W))
}

class WbUnitIn(implicit val conf:CAHPConfig) extends Bundle {
  val regWrite = Input(UInt(4.W))
  val regWriteData = Input(UInt(16.W))
  val regWriteEnable = Input(Bool())
}

class IdWbUnitPort (implicit val conf:CAHPConfig) extends Bundle {
  val idIn = new IdUnitIn
  val wbIn = new WbUnitIn
  val idEnable = Input(Bool())
  val wbEnable = Input(Bool())

  val exOut = Flipped(new ExUnitIn)
  val memOut = Flipped(new MemUnitIn)
  val wbOut = Flipped(new WbUnitIn)

  /*
  val debugRs = if (conf.test) Output(UInt(4.W)) else Output(UInt(0.W))
  val debugRd = if (conf.test) Output(UInt(4.W)) else Output(UInt(0.W))
  val debugRegWrite = if(conf.test) Output(Bool()) else Output(UInt(0.W))
  val debugImmLongState = if(conf.test) Output(Bool()) else Output(UInt(0.W))
  val testFinish = if (conf.test) Output(Bool()) else Output(UInt(0.W))
   */
  val testRegx8 = if (conf.test) Output(UInt(16.W)) else Output(UInt(0.W))
}

class DecoderPort(implicit val conf:CAHPConfig) extends Bundle {
  val in = new IdUnitIn

  val imm = Output(UInt(16.W))
  val pcImm = Output(UInt(16.W))
  val pcImmSel = Output(Bool())
  val rs1 = Output(UInt(4.W))
  val rs2 = Output(UInt(4.W))
  val rd = Output(UInt(4.W))
  val longInst = Output(Bool())
  val inASel = Output(Bool())
  val inBSel = Output(Bool())
  val exOut = Flipped(new ExUnitIn)
  val memOut = Flipped(new MemUnitIn)
  val wbOut = Flipped(new WbUnitIn)

  val testImmType = if(conf.test) Output(UInt(9.W)) else Output(UInt(0.W))
  val testPCImmType = if(conf.test) Output(UInt(2.W)) else Output(UInt(0.W))
}

object InstructionCategory {
  def InstR:UInt = 0.U(2.W)
  def InstI:UInt = 1.U(2.W)
  def InstM:UInt = 2.U(2.W)
  def InstJ:UInt = 3.U(2.W)
}

object ImmType {
  def SImm11:UInt  = "b000000001".U(9.W)
  def SImm10:UInt  = "b000000010".U(9.W)
  def SImm8:UInt   = "b000000100".U(9.W)
  def UImm8:UInt   = "b000001000".U(9.W)
  def UImm7:UInt   = "b000010000".U(9.W)
  def SImm6:UInt   = "b000100000".U(9.W)
  def UImm6:UInt   = "b001000000".U(9.W)
  def Imm4:UInt    = "b010000000".U(9.W)
  def Imm2:UInt    = "b100000000".U(9.W)
}

object PCImmType {
  def SImm10:UInt = "b01".U(2.W)
  def SImm11:UInt = "b10".U(2.W)
}

object PCOpcode {
  def SImm10:UInt = "b001".U(3.W)
  def SImm11:UInt = "b010".U(3.W)
}
class Decoder(implicit val conf:CAHPConfig) extends Module {

  def genImm(inst:UInt, immType:UInt):UInt = {
    val imm = Wire(UInt(16.W))
    imm:=DontCare
    when(immType === ImmType.SImm11){
      imm := Cat(Fill(6, inst(7)), inst(6), inst(23, 16), 0.U(1.W))
    }.elsewhen(immType === ImmType.SImm10){
      imm := Cat(Fill(7, inst(7)), inst(6), inst(23, 16))
    }.elsewhen(immType === ImmType.SImm8){
      imm := Cat(Fill(9, inst(23)), inst(22, 16))
    }.elsewhen(immType === ImmType.UImm8){
      imm := Cat(0.U(8.W), inst(23, 16))
    }.elsewhen(immType === ImmType.UImm7){
      imm := Cat(0.U(9.W), inst(7, 6), inst(15, 12), 0.U(1.W))
    }.elsewhen(immType === ImmType.SImm6){
      imm := Cat(Fill(11, inst(7)), inst(6), inst(15, 12))
    }.elsewhen(immType === ImmType.UImm6){
      imm := Cat(0.U(10.W), inst(7, 6), inst(15, 12))
    }.elsewhen(immType === ImmType.Imm4){
      imm := 4.U(16.W)
    }.elsewhen(immType === ImmType.Imm2){
      imm := 2.U(16.W)
    }
    imm
  }

  def genPCImm(inst:UInt, pcImmType:UInt):UInt = {
    val imm = Wire(UInt(16.W))
    imm:=DontCare
    when(pcImmType === PCImmType.SImm10){
      imm := Cat(Fill(7, inst(7)), inst(6), inst(23, 16))
    }.elsewhen(pcImmType === PCImmType.SImm11){
      imm := Cat(Fill(6, inst(15)), inst(15, 5))
    }
    imm
  }

  def getImmType(inst:UInt):UInt = {
    val immType = Wire(UInt(9.W))
    immType := DontCare
    when(inst(0) === 1.U) {
      when(inst(2, 1) === InstructionCategory.InstI){
          immType := ImmType.SImm8
      }.otherwise{
        immType := ImmType.SImm10
      }
    }.otherwise{
      when(inst(2, 1) === InstructionCategory.InstM){
        when(inst(5, 4) === 1.U){
          immType := ImmType.UImm7
        }.otherwise{
          immType := ImmType.SImm6
        }
      }.elsewhen(inst(2, 1) === InstructionCategory.InstI){
        when(inst(5, 4) === 0.U){
          immType := ImmType.SImm6
        }.otherwise{
          immType := ImmType.UImm6
        }
      }.elsewhen(inst(2, 1) === InstructionCategory.InstJ){
        immType := ImmType.Imm2
      }
    }
    immType
  }

  def getPCImmType(inst:UInt):UInt =  {
    val pcImmType = Wire(UInt(2.W))
    pcImmType := DontCare
    when(inst(0) === 1.U) {
      pcImmType := PCImmType.SImm10
    }.otherwise{
      pcImmType := PCImmType.SImm11
    }
    pcImmType
  }

  def getPCImmSel(inst:UInt):UInt = {
    val pcImmSel = Wire(Bool())
    pcImmSel := true.B
    when(inst(0) === 0.U){
      when(inst(3) === 0.U) {
        pcImmSel := false.B
      }
    }
    pcImmSel
  }

  def getPCOpcode(inst:UInt):UInt = {
    val pcOpcode = Wire(UInt(3.W))
    pcOpcode := 0.U(3.W)
    when(inst(0) === 1.U){
      when(inst(2, 1) === InstructionCategory.InstJ){
        pcOpcode := inst(5, 3)
      }
    }.otherwise{
      when(inst(2, 1) === InstructionCategory.InstJ){
        pcOpcode := 4.U(3.W)
      }
    }
    pcOpcode
  }

  def getExOpcode(inst:UInt): UInt = {
    val exOpcode = Wire(UInt(4.W))
    exOpcode := DontCare
    when(inst(2,1) === InstructionCategory.InstR) {
      exOpcode := Cat(inst(6, 3))
    }.elsewhen(inst(2, 1) === InstructionCategory.InstI){
      exOpcode := Cat(0.U(1.W), inst(5, 3))
    }.elsewhen(inst(2,1) === InstructionCategory.InstM){
      when((inst(5,4) === 3.U)||(inst(5, 0) === 4.U)){
        exOpcode := 8.U(4.W)
      }.otherwise{
        exOpcode := 0.U(4.W)
      }
    }.otherwise{
      when(inst(0) === 1.U){
        exOpcode := 1.U(4.W)
      }.otherwise{
        exOpcode := 0.U(4.W)
      }
    }
    exOpcode
  }

  def getMemWrite(inst:UInt): Bool = {
    val memWrite = Wire(Bool())
    memWrite := DontCare
    when(inst(2,1) === InstructionCategory.InstM){
      memWrite := inst(3) === 1.U
    }.otherwise{
      memWrite := false.B
    }
    memWrite
  }

  def getMemRead(inst:UInt): Bool = {
    val memRead = Wire(Bool())
    memRead := false.B
    when(inst(2, 1) === InstructionCategory.InstM){
      when(inst(3) === 0.U){
        when(inst(5, 0) != "b110101".U(6.W) && inst(5, 0) != "b110100".U(6.W) && inst(5, 0) != "b100".U(6.W)){
          memRead := true.B
        }
      }
    }
    memRead
  }

  def getMemByte(inst:UInt): Bool ={
    val byteEnable = Wire(Bool())
    byteEnable := DontCare
    when(inst(2,1) === InstructionCategory.InstM){
      byteEnable := false.B
      when(inst(0) === 1.U){
        //LB, LBU, SB
        when(inst(5, 3) === 4.U(3.W) ||
          inst(5, 3) === 0.U(3.W) ||
          inst(5, 3) === 1.U(3.W)){
          byteEnable := true.B
        }
      }
    }
    byteEnable
  }

  def getMemSignExt(inst:UInt): Bool = {
    val signExt = Wire(Bool())
    signExt := DontCare
    when(inst(2,1) === InstructionCategory.InstM){
      signExt := false.B
      //LB
      when(inst(0) === 1.U && inst(5) === 1.U){
        signExt := true.B
      }
    }
    signExt
  }

  def getRegWrite(inst:UInt): Bool = {
    val regWrite = Wire(Bool())

    when(inst(2,1) === InstructionCategory.InstM){
      regWrite := inst(3) != 1.U
    }.elsewhen(inst(2,1) === InstructionCategory.InstJ) {
      regWrite := (inst(4) === 1.U) && (inst(0) === 0.U)
    }.elsewhen(inst(7,0) === 0.U){
      regWrite := false.B
    }.otherwise{
      regWrite := true.B
    }
    regWrite
  }

  def getInASel(inst:UInt): Bool = {
    val inASel = Wire(Bool())
    val isLongInst:Bool = (inst(0) === 1.U)
    inASel := false.B
    when(!isLongInst){
      when(inst(2, 1) === InstructionCategory.InstJ){
        inASel := true.B
      }
    }
    inASel
  }

  def getInBSel(inst:UInt): Bool = {
    val inBSel = Wire(Bool())
    val isLongInst:Bool = (inst(0) === 1.U)
    when(inst(2, 1) === InstructionCategory.InstR){
      inBSel := false.B
    }.otherwise{
      inBSel := true.B
      when(isLongInst) {
        when(inst(2, 1) === InstructionCategory.InstJ){
          inBSel := false.B
        }
      }
    }
    inBSel
  }
  val io = IO(new DecoderPort)

  io.imm := genImm(io.in.inst, getImmType(io.in.inst))
  io.pcImm := genPCImm(io.in.inst, getPCImmType(io.in.inst))
  io.pcImmSel := getPCImmSel(io.in.inst)
  io.testImmType := getImmType(io.in.inst)
  io.testPCImmType := getPCImmType(io.in.inst)

  io.inASel := getInASel(io.in.inst)
  io.inBSel := getInBSel(io.in.inst)
  io.exOut.opcode := getExOpcode(io.in.inst)
  io.exOut.inA := DontCare
  io.exOut.inB := DontCare
  io.exOut.pcOpcode := getPCOpcode(io.in.inst)
  io.exOut.pcImm := DontCare
  io.exOut.pc := DontCare
  io.exOut.pcAdd := DontCare
  io.memOut.memRead := getMemRead(io.in.inst)
  io.memOut.memWrite := getMemWrite(io.in.inst)
  io.memOut.byteEnable := getMemByte(io.in.inst)
  io.memOut.signExt := getMemSignExt(io.in.inst)
  io.memOut.address := DontCare
  io.memOut.in := DontCare

  io.longInst := (io.in.inst(0) === 1.U)

  when(io.longInst) {
    io.rs1 := io.in.inst(15, 12)
    when(io.in.inst(2, 1) === InstructionCategory.InstM || io.in.inst(2, 1) === InstructionCategory.InstJ){
      io.rs2 := io.in.inst(11, 8)
    }.otherwise{
      io.rs2 := io.in.inst(19,16)
    }
  }.otherwise{
    when(io.in.inst(2, 1) === InstructionCategory.InstM){
      io.rs1 := 1.U(4.W)
      io.rs2 := io.in.inst(11, 8)
    }.otherwise{
      io.rs1 := io.in.inst(11, 8)
      io.rs2 := io.in.inst(15, 12)
    }
  }
  when(io.in.inst(2,1) === InstructionCategory.InstJ){
    io.rd := 0.U(4.W)
  }.otherwise{
    io.rd := io.in.inst(11, 8)
  }

  io.wbOut.regWrite := io.rd
  io.wbOut.regWriteEnable := getRegWrite(io.in.inst)
  io.wbOut.regWriteData := DontCare
}

class IdWbUnit(implicit val conf: CAHPConfig) extends Module {
  val io = IO(new IdWbUnitPort)

  val decoder = Module(new Decoder())
  val mainRegister = Module(new MainRegister())
  val pIdReg = RegInit(0.U.asTypeOf(new IdUnitIn))

  when(io.idEnable){
    pIdReg := io.idIn
  }
  decoder.io.in := pIdReg

  mainRegister.io.rs1 := decoder.io.rs1
  mainRegister.io.rs2 := decoder.io.rs2
  mainRegister.io.rd := io.wbIn.regWrite
  mainRegister.io.writeData := io.wbIn.regWriteData
  mainRegister.io.writeEnable := io.wbIn.regWriteEnable&&io.wbEnable

  io.exOut := decoder.io.exOut
  io.exOut.pc := pIdReg.pc
  when(decoder.io.pcImmSel){
    io.exOut.pcImm := decoder.io.pcImm
    io.exOut.pcAdd := true.B
  }.otherwise{
    io.exOut.pcImm := mainRegister.io.rs1Data
    io.exOut.pcAdd := false.B
  }
  when(!decoder.io.inASel){
    io.exOut.inA := mainRegister.io.rs1Data
  }.otherwise{
    io.exOut.inA := pIdReg.pc
  }
  when(!decoder.io.inBSel){
    io.exOut.inB := mainRegister.io.rs2Data
  }.otherwise{
    //LUI
    when(pIdReg.inst(5, 0) === "b000100".U(6.W)){
      io.exOut.inB := Cat(decoder.io.imm(5, 0), 0.U(10.W))
    }.otherwise{
      io.exOut.inB := decoder.io.imm
    }
  }

  io.memOut := decoder.io.memOut
  io.memOut.in := mainRegister.io.rs2Data

  io.wbOut := decoder.io.wbOut

  io.testRegx8 := mainRegister.io.testRegx8
  when(conf.debugId.B){
    printf("[ID] PC Address:0x%x\n", pIdReg.pc)
    printf("[ID] Instruction:0x%x\n", pIdReg.inst)
    printf("[ID] Imm:0x%x\n", decoder.io.imm)
    printf("[ID] RegWrite:0x%x\n", decoder.io.wbOut.regWrite)
  }
}
