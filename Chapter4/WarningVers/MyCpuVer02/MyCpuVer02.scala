/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package mylib

import spinal.core._
import spinal.lib._

import scala.math._

//Hardware definition
//------------------------------------
//程序计数器模块
case class PC(pcWidth:Int) extends Component{
  val io = new Bundle{
    val pc = out UInt(pcWidth bits)
    val ce = out UInt(1 bits)
  }
  val pcCounterReg = Reg(UInt(pcWidth bits))init(0)
  val ceReg = Reg(U"b0")init(0)
  io.pc := pcCounterReg
  io.ce := ceReg
  ceReg := 1
  when(ceReg === 0){
    pcCounterReg := 0
  }.otherwise{
    pcCounterReg := pcCounterReg + 4
  }
}
//------------------------------------
//一级流水寄存器
case class IF2ID(addrWidth:Int,instWidth:Int) extends Component{
  val io = new Bundle{
    val pc2reg = in UInt(addrWidth bits)
    val inst2reg = in UInt(instWidth bits)
    val reg2idPC = out UInt(addrWidth bits)
    val reg2idInst =out UInt(instWidth bits)
  }

  val pcReg = RegNext(io.pc2reg)init(0)
  val instReg = RegNext(io.inst2reg)init(0)

  io.reg2idPC := pcReg
  io.reg2idInst := instReg
}
//------------------------------------
//通用寄存器模块
case class regFile(regWidth:Int) extends Component{
  val io = new Bundle{
    val waddr = in UInt(5 bits)
    val wdata = in UInt(regWidth bits)
    val we = in UInt(1 bits)
    val raddr1 = in UInt(5 bits)
    val re1 = in UInt(1 bits)
    val raddr2 = in UInt(5 bits)
    val re2 = in UInt(1 bits)
    val rdata1 = out UInt(regWidth bits)
    val rdata2 = out UInt(regWidth bits)
  }

  val regs = Mem(UInt(32 bits),32)

  //------------------------------------
  //写操作
  when((io.we === 1) && (io.waddr =/= 0)){
    regs(io.waddr) := io.wdata
  }
  //------------------------------------
  //端口1读操作
  //有问题，少写一个判断，回头补。
  when((io.raddr1 === io.waddr) && (io.we === 1) && (io.re1 === 1)){
    io.rdata1 := io.wdata
  }.elsewhen(io.re1 === 1){
    io.rdata1 := regs(io.raddr1)
  }.otherwise{
    io.rdata1 := 0
  }
  //------------------------------------
  //端口2读操作
  //有问题，少写一个判断，回头补。
  when((io.raddr2 === io.waddr) && (io.we === 1) && (io.re2 === 1)){
    io.rdata2 := io.wdata
  }.elsewhen(io.re1 === 1){
    io.rdata2 := regs(io.raddr2)
  }.otherwise{
    io.rdata2 := 0
  }
}
//------------------------------------
//指令译码模块

case class ID(pcWidth:Int,instWidth:Int) extends Component{
  val io = new Bundle{
    val pc = in UInt(pcWidth bits)
    val inst = in UInt(instWidth bits)
    val reg1DataIn = in UInt(instWidth bits)
    val reg2DataIn = in UInt(instWidth bits)
    val reg1Read = out UInt(1 bits)
    val reg2Read = out UInt(1 bits)
    val reg1Addr = out UInt(5 bits)
    val reg2Addr = out UInt(5 bits)
    val aluOp = out UInt(8 bits)
    val aluse = out UInt(3 bits)
    val reg1 = out UInt(instWidth bits)
    val reg2 = out UInt(instWidth bits)
    val wd =  out UInt(5 bits)
    val wreg = out UInt(1 bits)
  }
  val reg1ReadReg = Reg(UInt(1 bits))init(0)
  val reg2ReadReg = Reg(UInt(1 bits))init(0)
  val reg1AddrReg = Reg(UInt(5 bits))init(0)
  val reg2AddrReg = Reg(UInt(5 bits))init(0)
  val aluOpReg = Reg(UInt(8 bits))init(0)
  val aluseReg = Reg(UInt(3 bits))init(0)
  val reg1Reg = Reg(UInt(instWidth bits))init(0)
  val reg2Reg = Reg(UInt(instWidth bits))init(0)
  val wdReg = Reg(UInt(5 bits))init(0)
  val wregReg = Reg(UInt(1 bits))init(0)

  val imm = Reg(UInt(32 bits))init(0)
  val instValid = Reg(UInt(1 bits))init(0)

  io.reg1Read := reg1ReadReg
  io.reg2Read := reg2ReadReg
  io.reg1Addr := reg1AddrReg
  io.reg2Addr := reg2AddrReg
  io.aluOp := aluOpReg
  io.aluse := aluseReg
  io.reg1 := reg1Reg
  io.reg2 := reg2Reg
  io.wd := wdReg
  io.wreg := wregReg
  
  reg1AddrReg := io.inst(25 downto 21)
  reg2AddrReg := io.inst(20 downto 16)

  switch(io.inst(31 downto 26)){
    is(U"6'b001101"){
      wregReg := 1
      aluOpReg := U"8'b00100101"
      aluseReg := 1
      reg1ReadReg := 1
      reg2ReadReg := 0
      imm := U"16'b0" @@ io.inst(15 downto 0)
      wdReg := io.inst(20 downto 16)
      instValid := 0
    }
    default{
      aluOpReg := 0
      aluseReg := 0
      wdReg := io.inst(15 downto 11)
      wregReg := 0
      instValid := 1
      reg1ReadReg := 0
      reg2ReadReg := 0
      imm := 0
    }
  }

  when(reg1ReadReg === 1){
    reg1Reg := io.reg1DataIn
  }.elsewhen(reg1ReadReg === 0){
    reg1Reg := imm
  }.otherwise{
    reg1Reg := 0
  }

  when(reg2ReadReg === 1){
    reg2Reg := io.reg2DataIn
  }.elsewhen(reg2ReadReg === 0){
    reg2Reg := imm
  }.otherwise{
    reg2Reg := 0
  }
}
//------------------------------------
//二级流水线寄存器
case class ID2EX(instWidth:Int) extends Component{
  val io = new Bundle{
    val aluOp2reg = in UInt(8 bits)
    val aluse2reg = in UInt(3 bits)
    val reg12reg = in UInt(instWidth bits)
    val reg22reg = in UInt(instWidth bits)
    val wd2reg = in UInt(5 bits)
    val wreg2reg = in UInt(1 bits)
    val reg2exAluOp = out UInt(8 bits)
    val reg2exAluse = out UInt(3 bits)
    val reg2exReg1 = out UInt(instWidth bits)
    val reg2exReg2 = out UInt(instWidth bits)
    val reg2exWd = out UInt(5 bits)
    val reg2exWreg = out UInt(1 bits)
  }

  val aluOpReg = RegNext(io.aluOp2reg)init(0)
  val aluseReg = RegNext(io.aluse2reg)init(0)
  val reg1Reg = RegNext(io.reg12reg)init(0)
  val reg2Reg = RegNext(io.reg22reg)init(0)
  val wdReg = RegNext(io.wd2reg)init(0)
  val wregReg = RegNext(io.wreg2reg)init(0)

  io.reg2exAluOp := aluOpReg
  io.reg2exAluse := aluseReg
  io.reg2exReg1 := reg1Reg
  io.reg2exReg2 := reg2Reg
  io.reg2exWd := wdReg
  io.reg2exWreg := wregReg
}

//------------------------------------
//指令执行模块
case class EX(instWidth:Int) extends Component{
  val io = new Bundle{
    val aluOp = in UInt(8 bits)
    val aluse = in UInt(3 bits)
    val reg1 = in UInt(instWidth bits)
    val reg2 = in UInt(instWidth bits)
    val wdIn = in UInt(5 bits)
    val wregIn = in UInt(1 bits)
    val wdOut = out UInt(5 bits)
    val wregOut = out UInt(1 bits)
    val wdata = out UInt(instWidth bits)
  }
  val logicOut = Reg(UInt(instWidth bits))init(0)

  switch(io.aluOp){
    is(U"8'b00100101"){
      logicOut := io.reg1 | io.reg2
    }
    default{
      logicOut := 0
    }
  }

  io.wdOut := io.wdIn
  io.wregOut := io.wregIn 
  switch(io.aluse){
    is(U"3'b001"){
      io.wdata := logicOut
    }
    default{
      io.wdata := 0
    }
  }
}

//------------------------------------
//三级流水线寄存器
case class EX2MEM(instWidth:Int) extends Component{
  val io = new Bundle{
    val wd2reg = in UInt(5 bits)
    val wreg2reg = in UInt(1 bits)
    val wdata2reg = in UInt(instWidth bits)
    val reg2MemWd = out UInt(5 bits)
    val reg2MemWreg = out UInt(1 bits)
    val reg2MemWdata = out UInt(instWidth bits)
  }
  val wdReg = RegNext(io.wd2reg)init(0)
  val wregReg = RegNext(io.wreg2reg)init(0)
  val wdataReg = RegNext(io.wdata2reg)init(0)

  io.reg2MemWd := wdReg
  io.reg2MemWreg := wregReg
  io.reg2MemWdata := wdataReg
}

//------------------------------------
//访存模块
case class MEM(instWidth:Int) extends Component{
  val io = new Bundle{
    val wdIn = in UInt(5 bits)
    val wregIn = in UInt(1 bits)
    val wdataIn = in UInt(instWidth bits)
    val wdOut = out UInt(5 bits)
    val wregOut = out UInt(1 bits)
    val wdataOut = out UInt(instWidth bits)
  }
  val wdReg = RegNext(io.wdIn)init(0)
  val wregReg = RegNext(io.wregIn)init(0)
  val wdataReg = RegNext(io.wdataIn)init(0)

  io.wdOut := wdReg
  io.wregOut := wregReg
  io.wdataOut := wdataReg
}

//------------------------------------
//四级流水线寄存器
case class MEM2WB(instWidth:Int) extends Component{
  val io = new Bundle{
    val wd2reg = in UInt(5 bits)
    val wreg2reg = in UInt(1 bits)
    val wdata2reg = in UInt(instWidth bits)
    val reg2wbWd = out UInt(5 bits)
    val reg2wbWreg = out UInt(1 bits)
    val reg2wbWdata = out UInt(instWidth bits)
  }
  val wdReg = RegNext(io.wd2reg)init(0)
  val wregReg = RegNext(io.wreg2reg)init(0)
  val wdataReg = RegNext(io.wdata2reg)init(0)

  io.reg2wbWd := wdReg
  io.reg2wbWreg := wregReg
  io.reg2wbWdata := wdataReg
}
//------------------------------------
//指令存储器模块
case class ROM(addrWidth:Int,instWidth:Int,regWidth:Int) extends Component {
  val io = new Bundle{
    val addr = in UInt(addrWidth bits)
    val ce = in UInt(1 bits)
    val inst = out UInt(instWidth bits)
  }
  //------------------------------------
  //加载ROM初始化数据
  val patch = "C:\\Users\\ZLXT\\Desktop\\SpinalTemplateSbt-master\\src\\main\\scala\\mylib\\rom.data"
  val source = new loadData
  val romData = source.loadRomData(patch) 
  //------------------------------------
  //充填数据
  val regSize = pow(2,regWidth).toInt
  def romTable = for(x <- 0 until regSize)yield{
    if(x < romData.size){
      U(romData(x).toLong,instWidth bits)
    }else{
      U(0,instWidth bits)
    }
  }
  //------------------------------------
  //定义ROM并初始化
  val rom  = Mem(UInt(instWidth bits),regSize)
  rom.init(romTable)

  when(io.ce === 1){
    io.inst := rom(io.addr((regWidth + 1) downto 2))
  }.otherwise{
    io.inst := 0
  }
}

//------------------------------------
//顶层设计
class MyTopLevel extends Component {
  val io = new Bundle{}

  val pcWidth,instWidth,addrWidth = 32
  val regWidth = 8

  val myPC = new PC(pcWidth)
  val myRom = new ROM(addrWidth ,instWidth,regWidth)
  val myIF2ID = new IF2ID(addrWidth, instWidth)
  val myID = new ID(pcWidth,instWidth)
  val myregFile = new regFile(instWidth)
  val myID2EX = new ID2EX(instWidth)
  val myEX = new EX(instWidth)
  val myEX2MEM = new EX2MEM(instWidth)
  val myMEM = new MEM(instWidth)
  val myMem2WB = new MEM2WB(instWidth)

  myIF2ID.io.pc2reg := myPC.io.pc
  myRom.io.addr := myPC.io.pc
  myRom.io.ce := myPC.io.ce
  myIF2ID.io.inst2reg := myRom.io.inst
  myID.io.inst := myIF2ID.io.reg2idInst
  myID.io.pc := myIF2ID.io.reg2idPC
  myID.io.reg1DataIn := myregFile.io.rdata1
  myID.io.reg2DataIn := myregFile.io.rdata2
  myID2EX.io.aluOp2reg := myID.io.aluOp
  myID2EX.io.aluse2reg := myID.io.aluse
  myID2EX.io.reg12reg := myID.io.reg1
  myID2EX.io.reg22reg := myID.io.reg2
  myID2EX.io.wd2reg := myID.io.wd
  myID2EX.io.wreg2reg := myID.io.wreg
  myEX.io.aluOp := myID2EX.io.reg2exAluOp
  myEX.io.aluse := myID2EX.io.reg2exAluse
  myEX.io.reg1 := myID2EX.io.reg2exReg1
  myEX.io.reg2 := myID2EX.io.reg2exReg2
  myEX.io.wdIn := myID2EX.io.reg2exWd
  myEX.io.wregIn := myID2EX.io.reg2exWreg
  myEX2MEM.io.wdata2reg := myEX.io.wdata
  myEX2MEM.io.wd2reg := myEX.io.wdOut
  myEX2MEM.io.wreg2reg := myEX.io.wregOut
  myMEM.io.wdataIn := myEX2MEM.io.reg2MemWdata
  myMEM.io.wdIn := myEX2MEM.io.reg2MemWd
  myMEM.io.wregIn := myEX2MEM.io.reg2MemWreg
  myMem2WB.io.wdata2reg := myMEM.io.wdataOut
  myMem2WB.io.wd2reg := myMEM.io.wdOut
  myMem2WB.io.wreg2reg := myMEM.io.wregOut
  myregFile.io.we := myMem2WB.io.reg2wbWreg
  myregFile.io.waddr := myMem2WB.io.reg2wbWd
  myregFile.io.wdata := myMem2WB.io.reg2wbWdata
  myregFile.io.re1 := myID.io.reg1Read
  myregFile.io.raddr1 := myID.io.reg1Addr
  myregFile.io.re2 := myID.io.reg2Read
  myregFile.io.raddr2 := myID.io.reg2Addr

}

//Generate the MyTopLevel's Verilog
object MyTopLevelVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new MyTopLevel)
  }
}
