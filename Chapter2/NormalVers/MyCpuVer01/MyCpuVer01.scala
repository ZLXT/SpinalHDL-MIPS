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


//Hardware definition

case class PC() extends Component{
  val io = new Bundle{
    val pc = out UInt(6 bits)
    val ce = out UInt(1 bits)
  }
  val pcCounterReg = Reg(U"6'b0")init(U"6'b0")
  val ceReg = Reg(U"b0")init(U"b0")
  io.pc := pcCounterReg
  io.ce := ceReg
  ceReg := 1
  when(ceReg === U"b0"){
    pcCounterReg := U"6'b0"
  }.otherwise{
    pcCounterReg := pcCounterReg + 1
  }
  
}

case class ROM() extends Component {
  val io = new Bundle{
    val addr = in UInt(6 bits)
    val ce = in UInt(1 bits)
    val inst = out UInt(32 bits)
  }

  val patch = "C:\\Users\\ZLXT\\Desktop\\SpinalTemplateSbt-master\\src\\main\\scala\\mylib\\rom.data"
  val source = new loadData
  val romData = source.loadRomData(patch) 

  def romTable = for(x <- romData)yield{
    U(x.toLong,32 bits)
  }

  val rom  = Mem(UInt(32 bits),initialContent = romTable)

  when(io.ce === U"b1"){
    io.inst := rom(io.addr) 
  }.otherwise{
    io.inst := 0
  }
}

//---------
class MyTopLevel extends Component {
  val io = new Bundle{
    val inst = out UInt(32 bits)
  }
  val myPC = new PC
  val myRom = new ROM
  myRom.io.addr := myPC.io.pc
  myRom.io.ce := myPC.io.ce
  io.inst := myRom.io.inst
}

//Generate the MyTopLevel's Verilog
object MyTopLevelVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new MyTopLevel)
  }
}
