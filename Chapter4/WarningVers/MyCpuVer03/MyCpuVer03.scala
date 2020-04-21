package mylib

import spinal.core._
import spinal.lib._

import scala.math._


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
    val imm = Reg(UInt(32 bits))init(0)
    val instValid = Reg(UInt(1 bits))init(0)

    val wReg = Reg(UInt(1 bits))init(0)
    val wd = Reg(UInt(5 bits))init(0)
    val alueOp = Reg(UInt(8 bits))init(0)
    val alusel = Reg(UInt(3 bits))init(0)

    val reg1Read = Reg(UInt(1 bits))init(0)
    val reg2Read = Reg(UInt(1 bits))init(0)
    val reg1Addr = Reg(UInt(5 bits))init(0)
    val reg2Addr = Reg(UInt(5 bits))init(0)
    val reg1 = Reg(UInt(instWidth bits))init(0)
    val reg2 = Reg(UInt(instWidth bits))init(0)

    val reg1Data = Reg(UInt(instWidth bits))init(0)
    val reg2Data = Reg(UInt(instWidth bits))init(0)

    instValid := 1
    imm := 0
    wReg := 0
    wd := flowReg1.instReg(15 downto 11)
    //wd := io.inst(15 downto 11)
    alueOp := 0
    alusel := 0
    reg1Read := 0
    reg2Read := 0

    reg1Addr := flowReg1.instReg(25 downto 21)
    reg2Addr := flowReg1.instReg(20 downto 16)
    //reg1Addr := io.inst(25 downto 21)
    //reg2Addr := io.inst(20 downto 16)
    //------------------------------------
    //问题：不知道为什么switch会晚一个时钟选择，因此不能用instReg的信号，只能将选择信号连接到寄存器前，向前推一个时钟。
    //需要设计为instReg的值来了以后立刻会激发switch选择，可以从instValid来看是否选择。
    switch(flowReg1.instReg(31 downto 26)){
    //switch(io.inst(31 downto 26)){
      is(U"6'b001101"){
        imm := U"16'b0" @@ flowReg1.instReg(15 downto 0)
        //imm := U"16'b0" @@ io.inst(15 downto 0)
        instValid := 0
        wReg := 1
        wd := flowReg1.instReg(20 downto 16)
        //wd := io.inst(20 downto 16)
        alueOp := U"8'b00100101"
        alusel := 1
        reg1Read := 1
        reg2Read := 0
      }
      default{

      } 
    }

    when(reg1Read === 1){
      reg1 := reg1Data
    }.elsewhen(reg1Read === 0){
      reg1 := imm
    }.otherwise{
      reg1 := 0
    }

    when(reg2Read === 1){
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
    val aluOp = RegNext(ID.alueOp)init(0)
    val alusel = RegNext(ID.alusel)init(0)
    val reg1 = RegNext(ID.reg1)init(0)
    val reg2 = RegNext(ID.reg2)init(0)
    val wd = RegNext(ID.wd)init(0)
    val wReg = RegNext(ID.wReg)init(0)
  }

//------------------------------------
//EX模块
  val EX = new Area{
    val logicOut = Reg(UInt(instWidth bits))init(0)
    val wReg = RegNext(flowReg2.wReg)init(0)
    val wd = RegNext(flowReg2.wd)init(0)
    val wdata = Reg(UInt(instWidth bits))init(0)

    switch(flowReg2.aluOp){
      is(U"8'b00100101"){
        logicOut := flowReg2.reg1 | flowReg2.reg2
      }
      default{
        logicOut := 0
      }
    }

    switch(flowReg2.alusel){
      is(U"3'b001"){
        wdata := logicOut
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
    val wd = RegNext(flowReg3.wd)init(0)
    val wReg = RegNext(flowReg3.wReg)init(0)
    val wdata = RegNext(flowReg3.wdata)init(0)
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