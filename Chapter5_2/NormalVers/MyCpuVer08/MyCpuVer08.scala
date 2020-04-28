package mylib

import spinal.core._
import spinal.lib._

import scala.math._

//------------------------------------
//除流水线寄存器,PC和通用寄存器模块外，均未使用寄存器，保障流各模块的功能可以在一个周期内执行完成。
case class Cpu(instWidth:Int,addrWidth:Int) extends Component{
  val io = new Bundle{
    val inst = in UInt(instWidth bits)
    val romCe = out UInt(1 bits)
    val romAddr = out UInt(addrWidth bits)
  }
    
//------------------------------------
//PC模块
  val PC = new Area{
    val pcCounter = Reg(UInt(32 bits))init(0)
    val ceReg = Reg(UInt(1 bits))init(0)
    ceReg := 1
    when(ceReg === 0){
      pcCounter := 0
    }.otherwise{
      pcCounter := pcCounter + 4
    }

    io.romCe := ceReg
    io.romAddr := pcCounter
  }

//------------------------------------
//一级流水寄存器
  val flowReg1 = new Area{
    val pcReg = RegNext(PC.pcCounter)init(0)
    val instReg = RegNext(io.inst)init(0)
  }
//------------------------------------
//ID模块
  val ID = new Area{
    val imm = UInt(32 bits)
    val instValid = UInt(1 bits)

    val wReg = UInt(1 bits)
    val wd = UInt(5 bits)

    val aluOp = UInt(8 bits)
    val aluSel = UInt(3 bits)

    val reg1Read = UInt(1 bits)
    val reg2Read = UInt(1 bits)
    val reg1Addr = UInt(5 bits)
    val reg2Addr = UInt(5 bits)
    val reg1 = UInt(32 bits)
    val reg2 = UInt(32 bits)

    val reg1Data = UInt(32 bits)
    val reg2Data = UInt(32 bits)

    val exWReg = UInt(1 bits)
    val exWData = UInt(32 bits)
    val exWd = UInt(5 bits)
    val memWReg = UInt(1 bits)
    val memWData = UInt(32 bits)
    val memWd = UInt(5 bits)

    reg1Addr := flowReg1.instReg(MIPS.RS)
    reg2Addr := flowReg1.instReg(MIPS.RT)

    //------------------------------------
    //switch中为寄存器赋值时，寄存器的值会在下一个时钟周期更新。为线类型赋值会立刻更新。
    switch(flowReg1.instReg){//OP3
      is(MIPS.OR){//OR
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.OR_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.AND){//AND
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.AND_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.XOR){//XOR
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.XOR_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.NOR){//NOR
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.NOR_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.SLLV){//SLLV
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.SLL_Op
        aluSel := MIPS.RES_Shift
        reg1Read := 1
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.SRLV){//SRLV
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.SRL_Op
        aluSel := MIPS.RES_Shift
        reg1Read := 1
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.SRAV){//SRAV
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.SRA_Op
        aluSel := MIPS.RES_Shift
        reg1Read := 1
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.SYNC){//SYNC
        imm := 0
        wReg :=1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.NOP_Op
        aluSel := MIPS.RES_Nop
        reg1Read := 0
        reg2Read := 1
        instValid := 0
      }
      is(MIPS.ORI){//ORI
        imm := U"16'b0" @@ flowReg1.instReg(MIPS.IMM)
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(MIPS.RT)
        aluOp := MIPS.OR_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 0
      }
      is(MIPS.ANDI){//ANDI
        imm := U"16'b0" @@ flowReg1.instReg(MIPS.IMM)
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(MIPS.RT)
        aluOp := MIPS.AND_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 0
      }
      is(MIPS.XORI){//XORI
        imm := U"16'b0" @@ flowReg1.instReg(MIPS.IMM)
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(MIPS.RT)
        aluOp := MIPS.XOR_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 0
      }
      is(MIPS.LUI){//LUI
        imm := flowReg1.instReg(MIPS.IMM) @@ U"16'b0"
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(MIPS.RT)
        aluOp := MIPS.OR_Op
        aluSel := MIPS.RES_Logic
        reg1Read := 1
        reg2Read := 0
      }
      is(MIPS.PREF){//PREF
        imm := 0
        instValid := 0
        wReg := 0
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.NOP_Op
        aluSel := MIPS.RES_Nop
        reg1Read := 0
        reg2Read := 0
      }
      is(MIPS.SLL){//SLL
        imm := (MIPS.SHIFT -> flowReg1.instReg(MIPS.SA),default -> false)
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.SLL_Op
        aluSel := MIPS.RES_Shift
        reg1Read := 0
        reg2Read := 1
      }
      is(MIPS.SRL){//SRL
        imm := (MIPS.SHIFT -> flowReg1.instReg(MIPS.SA),default -> false)
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.SRL_Op
        aluSel := MIPS.RES_Shift
        reg1Read := 0
        reg2Read := 1
      }
      is(MIPS.SRA){//SRA
        imm := (MIPS.SHIFT -> flowReg1.instReg(MIPS.SA),default -> false)
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := MIPS.SRA_Op
        aluSel := MIPS.RES_Shift
        reg1Read := 0
        reg2Read := 1
      }
      default{
        instValid := 1
        imm := 0
        wReg := 0
        wd := flowReg1.instReg(MIPS.RD)
        aluOp := 0
        aluSel := 0
        reg1Read := 0
        reg2Read := 0
      } 
    }


    when((reg1Read === 1) && (exWReg === 1) && (exWd === reg1Addr)){
      reg1 := exWData
    }.elsewhen((reg1Read === 1) && (memWReg === 1) && (memWd === reg1Addr)){
      reg1 := memWData
    }.elsewhen(reg1Read === 1){
      reg1 := reg1Data
    }.elsewhen(reg1Read === 0){
      reg1 := imm
    }.otherwise{
      reg1 := 0
    }

    when((reg2Read === 1) && (exWReg === 1) && (exWd === reg2Addr)){
      reg2 := exWData
    }.elsewhen((reg2Read === 1) && (memWReg === 1) && (memWd === reg2Addr)){
      reg2 := memWData
    }.elsewhen(reg2Read === 1){
      reg2 := reg2Data
    }.elsewhen(reg2Read === 0){
      reg2 := imm
    }.otherwise{
      reg2 := 0
    }

  }

//------------------------------------
//二级流水线寄存器
  val flowReg2 = new Area{
    val aluOp = RegNext(ID.aluOp)init(0)
    val aluSel = RegNext(ID.aluSel)init(0)
    val reg1 = RegNext(ID.reg1)init(0)
    val reg2 = RegNext(ID.reg2)init(0)
    val wd = RegNext(ID.wd)init(0)
    val wReg = RegNext(ID.wReg)init(0)
  }

//------------------------------------
//EX模块
  val EX = new Area{
    val logicOut = UInt(instWidth bits)
    val shiftOut = UInt(instWidth bits)

    val wReg = flowReg2.wReg
    val wd = flowReg2.wd
    val wdata = UInt(instWidth bits)

    ID.exWd := wd
    ID.exWData := wdata
    ID.exWReg := wReg

    switch(flowReg2.aluOp){
      is(MIPS.OR_Op){//OR
        logicOut := flowReg2.reg1 | flowReg2.reg2
      }
      is(MIPS.AND_Op){//AND
        logicOut := flowReg2.reg1 & flowReg2.reg2
      }
      is(MIPS.NOR_Op){//NOR
        logicOut := ~(flowReg2.reg1 | flowReg2.reg2)
      }
      is(MIPS.XOR_Op){//XOR
        logicOut := flowReg2.reg1 ^ flowReg2.reg2
      }
      default{
        logicOut := 0
      }
    }

    switch(flowReg2.aluOp){
      is(MIPS.SLL_Op){//SLL
        shiftOut := flowReg2.reg2 |<< flowReg2.reg1(MIPS.SHIFT)
      }
      is(MIPS.SRL_Op){//SRL
        shiftOut := flowReg2.reg2 |>> flowReg2.reg1(MIPS.SHIFT)
      }
      is(MIPS.SRA_Op){//SRA
        shiftOut := U(S(flowReg2.reg2) >> flowReg2.reg1(MIPS.SHIFT))
      }
      default{
        shiftOut:= 0
      }
    }

    switch(flowReg2.aluSel){
      is(MIPS.RES_Logic){
        wdata := logicOut
      }
      is(MIPS.RES_Shift){
        wdata := shiftOut
      }
      default{
        wdata := 0
      }
    }
  }
//------------------------------------
//三级流水线寄存器
  val flowReg3 = new Area{
    val wd = RegNext(EX.wd)init(0)
    val wReg = RegNext(EX.wReg)init(0)
    val wdata = RegNext(EX.wdata)init(0)
  }
//------------------------------------
//MEM模块
  val MEM = new Area{
    val wd = flowReg3.wd
    val wReg = flowReg3.wReg
    val wdata = flowReg3.wdata

    ID.memWData := wdata
    ID.memWReg := wReg
    ID.memWd := wd
  }
//------------------------------------
//四级流水线寄存器
  val flowReg4 = new Area{
    val wd = RegNext(MEM.wd)init(0)
    val wReg = RegNext(MEM.wReg)init(0)
    val wdata = RegNext(MEM.wdata)init(0)
  }

//------------------------------------
//通用寄存器模块
  val regFiles = new Area{
    val regs = Mem(UInt(32 bits),32)
    //------------------------------------
    //写操作
    when((flowReg4.wReg === 1) && (flowReg4.wd =/= 0)){
      regs(flowReg4.wd) := flowReg4.wdata
    }
    //------------------------------------
    //端口1读操作
    when(ID.reg1Addr === 0){
      ID.reg1Data := 0
    }.elsewhen((ID.reg1Addr === flowReg4.wd) && (flowReg4.wReg === 1) && (ID.reg1Read === 1)){
      ID.reg1Data := flowReg4.wdata
    }.elsewhen(ID.reg1Read === 1){
      ID.reg1Data := regs(ID.reg1Addr)
    }.otherwise{
      ID.reg1Data := 0
    }
    //------------------------------------
    //端口2读操作
    when(ID.reg2Addr === 0){
      ID.reg2Data := 0
    }.elsewhen((ID.reg2Addr === flowReg4.wd) && (flowReg4.wReg === 1) && (ID.reg2Read === 1)){
      ID.reg2Data := flowReg4.wdata
    }.elsewhen(ID.reg2Read === 1){
      ID.reg2Data := regs(ID.reg2Addr)
    }.otherwise{
      ID.reg2Data := 0
    }
  }

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
  val patch = "C:\\Users\\ZLXT\\Desktop\\SpinalTemplateSbt\\src\\main\\scala\\mylib\\rom.data"
  val source = new loadData
  val romData = source.loadRomData(patch) 
  //------------------------------------
  //用0来充填数据，使数据数量和ROM长度相同
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

class MyTopLevel extends Component {
  //val io = new Bundle{}
  val myRom = new ROM(32,32,8)
  val myCpu = new Cpu(32,32)
  myRom.io.ce := myCpu.io.romCe
  myRom.io.addr := myCpu.io.romAddr
  myCpu.io.inst := myRom.io.inst
}


object MyTopLevelVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new MyTopLevel)
  }
}