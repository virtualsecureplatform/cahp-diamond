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

import chisel3.{util, _}
import chisel3.util.Cat

object romCacheStateType {
  val romCacheMiss:Bool = false.B
  val romCacheLoaded:Bool = true.B
}

object romCacheStateMachine {
  val Loading:UInt = "b001".U(3.W)
  val Loaded:UInt = "b010".U(3.W)
  val NotLoaded:UInt = "b100".U(3.W)
}

class IfUnitIn(implicit val conf: CAHPConfig) extends Bundle {
  val romData = Input(UInt(conf.romCacheWidth.W))
  val jumpAddress = Input(UInt(conf.romAddrWidth.W))
  val jump = Input(Bool())
}

class IfUnitOut(implicit val conf: CAHPConfig) extends Bundle {
  val pcAddress = Output(UInt(conf.romAddrWidth.W))
  val romAddress = Output(UInt((conf.romAddrWidth-2).W))
  val instOut = Output(UInt(conf.longInstWidth.W))
  val stole = Output(Bool())
}

class IfUnitPort(implicit val conf: CAHPConfig) extends Bundle {
  val in = new IfUnitIn
  val out = new IfUnitOut
  val enable = Input(Bool())

  val testRomCacheState = if (conf.test) Output(Bool()) else Output(UInt(0.W))
  val testRomCache = if (conf.test) Output(UInt(32.W)) else Output(UInt(0.W))
}

class IfUnit(implicit val conf: CAHPConfig) extends Module {
  val io = IO(new IfUnitPort)
  val pc = Module(new PC)
  val stole = Wire(Bool())
  //val romAddr = RegInit(0.U((conf.romAddrWidth-2).W))
  val romAddr = Wire(UInt((conf.romAddrWidth-2).W))
  val cachedRomAddr = RegInit(0.U(conf.romAddrWidth.W))

  val cacheSt = RegInit(romCacheStateMachine.NotLoaded)

  // **** Register Declaration ****
  val romCache = RegInit(0.U(conf.romCacheWidth.W))
  val romCacheState = RegInit(romCacheStateType.romCacheMiss)


  // **** I/O Connection ****
  pc.io.jumpAddress := io.in.jumpAddress
  pc.io.jump := io.in.jump
  pc.io.enable := io.enable&&(!stole)
  pc.io.longInst := io.out.instOut(0) === 1.U

  io.out.pcAddress := pc.io.pcOut
  io.out.stole := stole

  // **** Test I/O Connection ****
  io.testRomCacheState := romCacheState
  io.testRomCache := romCache

  // **** Sequential Circuit ****
  when(io.enable) {
    romCacheState := romCacheState
    cacheSt := cacheSt
    when(romCache(0) === 0.U&&pc.io.pcOut(1,0) === 0.U&&cacheSt === romCacheStateMachine.Loaded){
      romCache := romCache
    }.otherwise{
      cachedRomAddr := io.out.romAddress
      romCache := io.in.romData
    }
    when(io.in.jump){
      romCacheState := romCacheStateType.romCacheMiss
      cacheSt := romCacheStateMachine.NotLoaded
    }.elsewhen(romCacheState === romCacheStateType.romCacheMiss) {
      when(!io.in.jump) {
        when(stole){
          romCacheState := romCacheStateType.romCacheLoaded
          cacheSt := romCacheStateMachine.Loaded
        }.elsewhen(pc.io.longInst === 1.U && pc.io.pcOut(1, 0) === 1.U){
          romCacheState := romCacheStateType.romCacheLoaded
          cacheSt := romCacheStateMachine.Loading
        }.elsewhen(pc.io.longInst === 0.U && pc.io.pcOut(1, 0) === 2.U){
          romCacheState := romCacheStateType.romCacheLoaded
          cacheSt := romCacheStateMachine.Loading
        }.otherwise{
          romCacheState := romCacheStateType.romCacheLoaded
          cacheSt := romCacheStateMachine.Loaded
        }
      }
      when(cacheSt === romCacheStateMachine.Loading){
        cacheSt := romCacheStateMachine.Loaded
      }
    }
  }

  when(cacheSt != romCacheStateMachine.Loaded){
    romAddr := pc.io.pcOut(conf.romAddrWidth-1, 2)
  }.otherwise{
    romAddr := pc.io.pcOut(conf.romAddrWidth-1, 2) + 1.U
  }
  io.out.romAddress := romAddr


  // **** Combination Circuit ****
  def getInstOpByte(pc:UInt, block:UInt):UInt = {
    val res = Wire(UInt(8.W))
    when(pc(1,0) === 0.U){
      res := block(7, 0)
    }.elsewhen(pc(1,0) === 1.U){
      res := block(15, 8)
    }.elsewhen(pc(1,0) === 2.U){
      res := block(23, 16)
    }.otherwise {
      res := block(31, 24)
    }
    res
  }

  def getInst(pc:UInt, upperBlock:UInt, lowerBlock:UInt):UInt = {
    val inst = Wire(UInt(24.W))
    when(pc(1,0) === 0.U){
      inst := Cat(lowerBlock(23, 16), lowerBlock(15, 8), lowerBlock(7, 0))
    }.elsewhen(pc(1,0) === 1.U){
      inst := Cat(lowerBlock(31, 24), lowerBlock(23, 16), lowerBlock(15, 8))
    }.elsewhen(pc(1,0) === 2.U){
      inst := Cat(upperBlock(7, 0), lowerBlock(31, 24), lowerBlock(23, 16))
    }.otherwise {
      inst := Cat(upperBlock(15, 8), upperBlock(7, 0), lowerBlock(31, 24))
    }
    inst
  }

  stole := false.B
  when(cacheSt === romCacheStateMachine.NotLoaded) {
    val isInstLong: Bool = Wire(Bool())
    isInstLong := getInstOpByte(io.out.pcAddress, io.in.romData)(0) === 1.U
    io.out.instOut := getInst(io.out.pcAddress, 0.U, io.in.romData)

    when((io.out.pcAddress(1, 0) === 3.U) || ((io.out.pcAddress(1) === 1.U) && isInstLong)) {
      stole := true.B
      io.out.instOut := 0.U(24.W)
    }
  }.elsewhen(cacheSt === romCacheStateMachine.Loading){
    io.out.instOut := getInst(io.out.pcAddress, 0.U, io.in.romData)
  }.otherwise{
    io.out.instOut := getInst(io.out.pcAddress, io.in.romData, romCache)
  }

  when(conf.debugIf.B){
    printf("\n[IF]PC Address:0x%x\n", io.out.pcAddress)
    printf("[IF] Instruction Out:%x\n", io.out.instOut)
    printf("[IF] Stole:%d\n", io.out.stole)
    printf("[IF] CacheState:%d\n", cacheSt)
    printf("[IF] jump:%d\n", io.in.jump)
    printf("[IF] RomAddress:%d\n", io.out.romAddress)
    printf("[IF] RomData:0x%x\n", io.in.romData)
    printf("[IF] RomCache:0x%x\n", romCache)
    printf("[IF] CachedRomAddr:0x%x\n", cachedRomAddr)
  }
}

class PCPort(implicit val conf: CAHPConfig) extends Bundle {
  val jumpAddress = Input(UInt(conf.romAddrWidth.W))
  val longInst = Input(Bool())
  val jump = Input(Bool())
  val enable = Input(Bool())
  val pcOut = Output(UInt(conf.romAddrWidth.W))
  val nextPcOut = Output(UInt(conf.romAddrWidth.W))
}

class PC(implicit val conf: CAHPConfig) extends Module {
  val io = IO(new PCPort)

  //**** Register Declaration ****
  val regPC = RegInit(0.U(conf.romAddrWidth.W))


  //**** I/O Connection ****
  io.pcOut := regPC


  //**** Sequential Circuit ****
  regPC := regPC
  when(io.enable) {
    when(io.jump === false.B) {
      when(io.longInst === true.B){
        regPC := regPC + 3.U
      }.otherwise{
        regPC := regPC + 2.U
      }
    }.otherwise {
      regPC := io.jumpAddress
    }
  }
  when(io.jump === false.B) {
    when(io.longInst === true.B){
      io.nextPcOut := regPC + 3.U
    }.otherwise{
      io.nextPcOut := regPC + 2.U
    }
  }.otherwise {
    io.nextPcOut := io.jumpAddress
  }
}
