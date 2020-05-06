package mylib

import spinal.core._
import spinal.lib._

import scala.math._

//------------------------------------
//除流水线寄存器,PC和通用寄存器模块外，均未使用寄存器，保障流各模块的功能可以在一个周期内执行完成。
case class Cpu(instWidth:Int,addrWidth:Int) extends Component{
  val io = new Bundle{
    val inst    = in UInt(instWidth bits)
    val romCe   = out UInt(1 bits)
    val romAddr = out UInt(addrWidth bits)
  }
    
//------------------------------------
//flowCtrl模块
  val flowCtrl = new Area{
    val stallFromID = UInt(1 bits)
    val stallFromEX = UInt(1 bits)
    val stall       = UInt(5 bits)

    when(stallFromEX === 1){
      stall := U"5'b00111"
    }.elsewhen(stallFromID === 1){
      stall := U"5'b00011"
    }.otherwise{
      stall := 0
    }
  }

//------------------------------------
//PC模块
  val PC = new Area{
    val pcCounter = Reg(UInt(32 bits))init(0)
    val ceReg     = UInt(1 bits)
    val stall     = flowCtrl.stall

    ceReg := 1
    when(stall(0) === False){
      pcCounter := pcCounter + 4
    }.otherwise{
      //KeepValue
    }

    io.romCe   := ceReg
    io.romAddr := pcCounter
  }

//------------------------------------
//一级流水寄存器
  val flowReg1 = new Area{
    val stall   = flowCtrl.stall

    val pcReg   = Reg(UInt(instWidth bits))init(0)
    val instReg = Reg(UInt(instWidth bits))init(0)

    when((stall(1) === True)){
      //KeepValue
    }.elsewhen(stall(1) === False){
      pcReg   := PC.pcCounter
      instReg := io.inst
    }.otherwise{
      pcReg   := 0
      instReg := 0
    }
  }
//------------------------------------
//ID模块
  val ID = new Area{
    val imm       = UInt(32 bits)
    val signBit   = UInt(16 bits)

    val instValid = UInt(1 bits)
    val stallReq  = UInt(1 bits)

    val wReg      = UInt(1 bits)
    val wd        = UInt(5 bits)

    val aluOp     = UInt(8 bits)
    val aluSel    = UInt(3 bits)

    val reg1Read  = UInt(1 bits)
    val reg2Read  = UInt(1 bits)
    val reg1Addr  = UInt(5 bits)
    val reg2Addr  = UInt(5 bits)
    val reg1      = UInt(32 bits)
    val reg2      = UInt(32 bits)

    val reg1Data  = UInt(32 bits)
    val reg2Data  = UInt(32 bits)
    //------------------------------------
    //数据前推
    val exWReg   = UInt(1 bits)
    val exWData  = UInt(32 bits)
    val exWd     = UInt(5 bits)
    val memWReg  = UInt(1 bits)
    val memWData = UInt(32 bits)
    val memWd    = UInt(5 bits)

    //------------------------------------
    //暂时设定为0
    stallReq := 0
    flowCtrl.stallFromID := stallReq

    signBit  := (default -> flowReg1.instReg(15))

    reg1Addr := flowReg1.instReg(MIPS.RS)
    reg2Addr := flowReg1.instReg(MIPS.RT)

    //------------------------------------
    //switch中为寄存器赋值时，寄存器的值会在下一个时钟周期更新。为线类型赋值会立刻更新。
    switch(flowReg1.instReg){//OP3
      is(MIPS.NOP){
        instValid := 1
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := 0
        aluSel    := 0
        reg1Read  := 0
        reg2Read  := 0
      }
      is(MIPS.OR){//OR
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.OR_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.AND){//AND
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.AND_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.XOR){//XOR
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.XOR_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.NOR){//NOR
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.NOR_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SLLV){//SLLV
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SLL_Op
        aluSel    := MIPS.RES_Shift
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SRLV){//SRLV
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SRL_Op
        aluSel    := MIPS.RES_Shift
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SRAV){//SRAV
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SRA_Op
        aluSel    := MIPS.RES_Shift
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SYNC){//SYNC
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.NOP_Op
        aluSel    := MIPS.RES_Nop
        reg1Read  := 0
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.ORI){//ORI
        imm       := U"16'b0" @@ flowReg1.instReg(MIPS.IMM)
        instValid := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.OR_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 0
      }
      is(MIPS.ANDI){//ANDI
        imm       := U"16'b0" @@ flowReg1.instReg(MIPS.IMM)
        instValid := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.AND_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 0
      }
      is(MIPS.XORI){//XORI
        imm       := U"16'b0" @@ flowReg1.instReg(MIPS.IMM)
        instValid := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.XOR_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 0
      }
      is(MIPS.LUI){//LUI
        imm       := flowReg1.instReg(MIPS.IMM) @@ U"16'b0"
        instValid := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.OR_Op
        aluSel    := MIPS.RES_Logic
        reg1Read  := 1
        reg2Read  := 0
      }
      is(MIPS.PREF){//PREF
        imm       := 0
        instValid := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.NOP_Op
        aluSel    := MIPS.RES_Nop
        reg1Read  := 0
        reg2Read  := 0
      }
      is(MIPS.SLL){//SLL
        imm       := (MIPS.SHIFT -> flowReg1.instReg(MIPS.SA),default -> false)
        instValid := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SLL_Op
        aluSel    := MIPS.RES_Shift
        reg1Read  := 0
        reg2Read  := 1
      }
      is(MIPS.SRL){//SRL
        imm       := (MIPS.SHIFT -> flowReg1.instReg(MIPS.SA),default -> false)
        instValid := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SRL_Op
        aluSel    := MIPS.RES_Shift
        reg1Read  := 0
        reg2Read  := 1
      }
      is(MIPS.SRA){//SRA
        imm       := (MIPS.SHIFT -> flowReg1.instReg(MIPS.SA),default -> false)
        instValid := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SRA_Op
        aluSel    := MIPS.RES_Shift
        reg1Read  := 0
        reg2Read  := 1
      }
      is(MIPS.MFHI){//MFHI
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MFHI_Op
        aluSel    := MIPS.RES_MOVE
        reg1Read  := 0
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.MFLO){//MFLO
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MFLO_Op
        aluSel    := MIPS.RES_MOVE
        reg1Read  := 0
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.MTHI){//MTHI
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MTHI_Op
        aluSel    := MIPS.RES_Nop
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.MTLO){//MTLO
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MTLO_Op
        aluSel    := MIPS.RES_Nop
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.MOVN){//MOVN
        imm       := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MOVN_Op
        aluSel    := MIPS.RES_MOVE
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
        when(reg2 =/= 0){
          wReg    := 1
        }.otherwise{
          wReg    := 0
        }
      }
      is(MIPS.MOVZ){//MOVZ
        imm       := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MOVZ_Op
        aluSel    := MIPS.RES_MOVE
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
        when(reg2 === 0){
          wReg    := 1
        }.otherwise{
          wReg    := 0
        }
      }
      is(MIPS.SLT){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SLT_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SLTU){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SLTU_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.ADD){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.ADD_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.ADDU){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.ADDU_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SUB){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SUB_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SUBU){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.SUBU_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.MULT){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MULT_Op
        aluSel    := MIPS.RES_Nop
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.MULTU){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MULTU_Op
        aluSel    := MIPS.RES_Nop
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.SLTI){
        imm       := signBit @@ flowReg1.instReg(MIPS.IMM)
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.SLT_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.SLTIU){
        imm       := signBit @@ flowReg1.instReg(MIPS.IMM)
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.SLTU_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.ADDI){
        imm       := signBit @@ flowReg1.instReg(MIPS.IMM)
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.ADDI_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.ADDIU){
        imm       := signBit @@ flowReg1.instReg(MIPS.IMM)
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RT)
        aluOp     := MIPS.ADDIU_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.CLZ){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.CLZ_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.CLO){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.CLO_Op
        aluSel    := MIPS.RES_ARITHMETIC
        reg1Read  := 1
        reg2Read  := 0
        instValid := 0
      }
      is(MIPS.MUL){
        imm       := 0
        wReg      := 1
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MUL_Op
        aluSel    := MIPS.RES_MUL
        reg1Read  := 1
        reg2Read  := 1
        instValid := 0
      }
      is(MIPS.MADD){
        instValid := 0
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MADD_Op
        aluSel    := MIPS.RES_MUL
        reg1Read  := 1
        reg2Read  := 1
      }
      is(MIPS.MADDU){
        instValid := 0
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MADDU_Op
        aluSel    := MIPS.RES_MUL
        reg1Read  := 1
        reg2Read  := 1
      }
      is(MIPS.MSUB){
        instValid := 0
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MSUB_Op
        aluSel    := MIPS.RES_MUL
        reg1Read  := 1
        reg2Read  := 1
      }
      is(MIPS.MSUBU){
        instValid := 0
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := MIPS.MSUBU_Op
        aluSel    := MIPS.RES_MUL
        reg1Read  := 1
        reg2Read  := 1
      }
      default{
        instValid := 1
        imm       := 0
        wReg      := 0
        wd        := flowReg1.instReg(MIPS.RD)
        aluOp     := 0
        aluSel    := 0
        reg1Read  := 0
        reg2Read  := 0
      } 
    }

    //------------------------------------
    //数据前推具有优先级
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
    val stall  = flowCtrl.stall

    val aluOp  = Reg(UInt(8 bits))init(0)
    val aluSel = Reg(UInt(3 bits))init(0)
    val wd     = Reg(UInt(5 bits))init(0)
    val wReg   = Reg(UInt(1 bits))init(0)
    val reg1   = Reg(UInt(instWidth bits))init(0)
    val reg2   = Reg(UInt(instWidth bits))init(0)

    when((stall(2) === True)){
      //KeepValue
    }.elsewhen(stall(2) === False){
      aluOp  := ID.aluOp
      aluSel := ID.aluSel
      wd     := ID.wd
      wReg   := ID.wReg
      reg1   := ID.reg1
      reg2   := ID.reg2
    }.otherwise{
      aluOp  := 0
      aluSel := 0
      wd     := 0
      wReg   := 0
      reg1   := 0
      reg2   := 0
    }
  }

//------------------------------------
//EX模块
  val EX = new Area{
    val stallReq  = UInt(1 bits)
    val stallReq4MOp = UInt(1 bits)

    val logicOut = UInt(instWidth bits)
    val shiftOut = UInt(instWidth bits)
    val moveRes  = UInt(instWidth bits)

    val wReg     = UInt(1 bits)
    val wd       = flowReg2.wd
    val wData    = UInt(instWidth bits)
    //------------------------------------
    //HILO模块返回值
    val HIData = UInt(instWidth bits)
    val LOData = UInt(instWidth bits)
    //------------------------------------
    //HILO中间值，保存前推数据的结果
    val HISwitch = UInt(instWidth bits)
    val LOSwitch = UInt(instWidth bits)
    //------------------------------------
    //EX模块HILO相关信号输出
    val wdHILO = UInt(1 bits)
    val HI     = UInt(instWidth bits)
    val LO     = UInt(instWidth bits)
    //------------------------------------
    //HILO数据前推
    val memWdHILO = UInt(1 bits)
    val memHI     = UInt(instWidth bits)
    val memLO     = UInt(instWidth bits)
    val wbWdHILO  = UInt(1 bits)
    val wbHI      = UInt(instWidth bits)
    val wbLO      = UInt(instWidth bits)
    //------------------------------------
    //运算状态
    val ovSum         = UInt(1 bits)
    val reg1Eqreg2    = UInt(1 bits)
    val reg1Ltreg2    = UInt(1 bits)
    val armthmeticres = UInt(instWidth bits)
    val reg2Mux       = UInt(instWidth bits)
    val reg1Not       = UInt(instWidth bits)
    val resultSum     = UInt(instWidth bits)
    val opData1Mult   = UInt(instWidth bits)
    val opData2Mult   = UInt(instWidth bits)
    val HILOTemp      = UInt(2*instWidth bits)
    val mulRes        = UInt(2*instWidth bits)
    val mulResWb      = UInt(2*instWidth bits)
    //------------------------------------
    //多周期运行变量
    val cntIn       = UInt(2 bits)
    val cntOut      = UInt(2 bits)
    val HILOTemp1   = UInt(2*instWidth bits)
    val HILOTempIn  = UInt(2*instWidth bits)
    val HILOTempOut = UInt(2*instWidth bits)

    ID.exWd    := wd
    ID.exWData := wData
    ID.exWReg  := wReg

    //------------------------------------
    //进行运算
    reg2Mux := ((flowReg2.aluOp === MIPS.SUB_Op) ||
                (flowReg2.aluOp === MIPS.SUBU_Op) ||
                (flowReg2.aluOp === MIPS.SLT_Op)) ?
                (~flowReg2.reg2 + 1) | flowReg2.reg2
    
    resultSum := flowReg2.reg1 + reg2Mux

    ovSum := (((!flowReg2.reg1(31) & !reg2Mux(31)) & resultSum(31)) || 
              ((flowReg2.reg1(31) & reg2Mux(31)) & (!resultSum(31)))).asUInt
    
    reg1Ltreg2 := ((flowReg2.aluOp === MIPS.SLT_Op) ? 
                    ((flowReg2.reg1(31) & !flowReg2.reg2(31)) ||
                    (!flowReg2.reg1(31) & !flowReg2.reg2(31) & resultSum(31)) ||
                    (flowReg2.reg1(31) & flowReg2.reg2(31) & resultSum(31)))
                    | (flowReg2.reg1 < flowReg2.reg2)).asUInt

    reg1Not := ~flowReg2.reg1

    //------------------------------------
    //计算arithmeticres的值
    switch(flowReg2.aluOp){
      is(MIPS.SLT_Op,MIPS.SLTU_Op){
        armthmeticres := reg1Ltreg2.resized
      }
      is(MIPS.ADD_Op,MIPS.ADDU_Op,MIPS.ADDI_Op,MIPS.ADDIU_Op){
        armthmeticres := resultSum
      }
      is(MIPS.SUB_Op,MIPS.SUBU_Op){
        armthmeticres := resultSum
      }
      is(MIPS.CLZ_Op){
        //------------------------------------
        //SpinalHDL似乎不支持cond ? num1 | num2这种运算的级联操作
        /*
        armthmeticres :=  (flowReg2.reg1(31) ? 0 | flowReg2.reg1(30) ? 1  |
                            flowReg2.reg1(29) ? 2 | flowReg2.reg1(28) ? 3  |
                            flowReg2.reg1(27) ? 4 | flowReg2.reg1(26) ? 5  |
                            flowReg2.reg1(25) ? 6 | flowReg2.reg1(24) ? 7  |
                            flowReg2.reg1(23) ? 8 | flowReg2.reg1(22) ? 9  |
                            flowReg2.reg1(21) ? 10 | flowReg2.reg1(20) ? 11 |
                            flowReg2.reg1(19) ? 12 | flowReg2.reg1(18) ? 13 |
                            flowReg2.reg1(17) ? 14 | flowReg2.reg1(16) ? 15 |
                            flowReg2.reg1(15) ? 16 | flowReg2.reg1(14) ? 17 |
                            flowReg2.reg1(13) ? 18 | flowReg2.reg1(12) ? 19 |
                            flowReg2.reg1(11) ? 20 | flowReg2.reg1(10) ? 21 |
                            flowReg2.reg1(9) ? 22 | flowReg2.reg1(8) ? 23 |
                            flowReg2.reg1(7) ? 24 | flowReg2.reg1(6) ? 25 |
                            flowReg2.reg1(5) ? 26 | flowReg2.reg1(4) ? 27 |
                            flowReg2.reg1(3) ? 28 | flowReg2.reg1(2) ? 29 |
                            flowReg2.reg1(1) ? 30 | flowReg2.reg1(0) ? 31 | 32).asUInt
        */
        when(flowReg2.reg1(31)){
          armthmeticres := 0
        }.elsewhen(flowReg2.reg1(30)){
          armthmeticres := 1
        }.elsewhen(flowReg2.reg1(29)){
          armthmeticres := 2
        }.elsewhen(flowReg2.reg1(28)){
          armthmeticres := 3
        }.elsewhen(flowReg2.reg1(27)){
          armthmeticres := 4
        }.elsewhen(flowReg2.reg1(26)){
          armthmeticres := 5
        }.elsewhen(flowReg2.reg1(25)){
          armthmeticres := 6
        }.elsewhen(flowReg2.reg1(24)){
          armthmeticres := 7
        }.elsewhen(flowReg2.reg1(23)){
          armthmeticres := 8
        }.elsewhen(flowReg2.reg1(22)){
          armthmeticres := 9
        }.elsewhen(flowReg2.reg1(21)){
          armthmeticres := 10
        }.elsewhen(flowReg2.reg1(20)){
          armthmeticres := 11
        }.elsewhen(flowReg2.reg1(19)){
          armthmeticres := 12
        }.elsewhen(flowReg2.reg1(18)){
          armthmeticres := 13
        }.elsewhen(flowReg2.reg1(17)){
          armthmeticres := 14
        }.elsewhen(flowReg2.reg1(16)){
          armthmeticres := 15
        }.elsewhen(flowReg2.reg1(15)){
          armthmeticres := 16
        }.elsewhen(flowReg2.reg1(14)){
          armthmeticres := 17
        }.elsewhen(flowReg2.reg1(13)){
          armthmeticres := 18
        }.elsewhen(flowReg2.reg1(12)){
          armthmeticres := 19
        }.elsewhen(flowReg2.reg1(11)){
          armthmeticres := 20
        }.elsewhen(flowReg2.reg1(10)){
          armthmeticres := 21
        }.elsewhen(flowReg2.reg1(9)){
          armthmeticres := 22
        }.elsewhen(flowReg2.reg1(8)){
          armthmeticres := 23
        }.elsewhen(flowReg2.reg1(7)){
          armthmeticres := 24
        }.elsewhen(flowReg2.reg1(6)){
          armthmeticres := 25
        }.elsewhen(flowReg2.reg1(5)){
          armthmeticres := 26
        }.elsewhen(flowReg2.reg1(4)){
          armthmeticres := 27
        }.elsewhen(flowReg2.reg1(3)){
          armthmeticres := 28
        }.elsewhen(flowReg2.reg1(2)){
          armthmeticres := 29
        }.elsewhen(flowReg2.reg1(1)){
          armthmeticres := 30
        }.elsewhen(flowReg2.reg1(0)){
          armthmeticres := 31
        }.otherwise{
          armthmeticres := 32
        }
      }
      is(MIPS.CLO_Op){
        /*
        armthmeticres := (reg1Not(31) ? 0 | reg1Not(30) ? 1 |
                            reg1Not(29) ? 2 | reg1Not(28) ? 3 |
                            reg1Not(27) ? 4 | reg1Not(26) ? 5 |
                            reg1Not(25) ? 6 | reg1Not(24) ? 7 |
                            reg1Not(23) ? 8 | reg1Not(22) ? 9 |
                            reg1Not(21) ? 10 | reg1Not(20) ? 11 |
                            reg1Not(19) ? 12 | reg1Not(18) ? 13 |
                            reg1Not(17) ? 14 | reg1Not(16) ? 15 |
                            reg1Not(15) ? 16 | reg1Not(14) ? 17 |
                            reg1Not(13) ? 18 | reg1Not(12) ? 19 |
                            reg1Not(11) ? 20 | reg1Not(10) ? 21 |
                            reg1Not(9) ? 22 | reg1Not(8) ? 23 |
                            reg1Not(7) ? 24 | reg1Not(6) ? 25 |
                            reg1Not(5) ? 26 | reg1Not(4) ? 27 |
                            reg1Not(3) ? 28 | reg1Not(2) ? 29 |
                            reg1Not(1) ? 30 | reg1Not(0) ? 31 | 32).asUInt
          */
        when(reg1Not(31)){
          armthmeticres := 0
        }.elsewhen(reg1Not(30)){
          armthmeticres := 1
        }.elsewhen(reg1Not(29)){
          armthmeticres := 2
        }.elsewhen(reg1Not(28)){
          armthmeticres := 3
        }.elsewhen(reg1Not(27)){
          armthmeticres := 4
        }.elsewhen(reg1Not(26)){
          armthmeticres := 5
        }.elsewhen(reg1Not(25)){
          armthmeticres := 6
        }.elsewhen(reg1Not(24)){
          armthmeticres := 7
        }.elsewhen(reg1Not(23)){
          armthmeticres := 8
        }.elsewhen(reg1Not(22)){
          armthmeticres := 9
        }.elsewhen(reg1Not(21)){
          armthmeticres := 10
        }.elsewhen(reg1Not(20)){
          armthmeticres := 11
        }.elsewhen(reg1Not(19)){
          armthmeticres := 12
        }.elsewhen(reg1Not(18)){
          armthmeticres := 13
        }.elsewhen(reg1Not(17)){
          armthmeticres := 14
        }.elsewhen(reg1Not(16)){
          armthmeticres := 15
        }.elsewhen(reg1Not(15)){
          armthmeticres := 16
        }.elsewhen(reg1Not(14)){
          armthmeticres := 17
        }.elsewhen(reg1Not(13)){
          armthmeticres := 18
        }.elsewhen(reg1Not(12)){
          armthmeticres := 19
        }.elsewhen(reg1Not(11)){
          armthmeticres := 20
        }.elsewhen(reg1Not(10)){
          armthmeticres := 21
        }.elsewhen(reg1Not(9)){
          armthmeticres := 22
        }.elsewhen(reg1Not(8)){
          armthmeticres := 23
        }.elsewhen(reg1Not(7)){
          armthmeticres := 24
        }.elsewhen(reg1Not(6)){
          armthmeticres := 25
        }.elsewhen(reg1Not(5)){
          armthmeticres := 26
        }.elsewhen(reg1Not(4)){
          armthmeticres := 27
        }.elsewhen(reg1Not(3)){
          armthmeticres := 28
        }.elsewhen(reg1Not(2)){
          armthmeticres := 29
        }.elsewhen(reg1Not(1)){
          armthmeticres := 30
        }.elsewhen(reg1Not(0)){
          armthmeticres := 31
        }.otherwise{
          armthmeticres := 32
        }
      }
      default{
        armthmeticres := 0
      }
    }

    //------------------------------------
    //乘法运算
    opData1Mult := ((((flowReg2.aluOp === MIPS.MUL_Op) || (flowReg2.aluOp === MIPS.MULT_Op)
                    || (flowReg2.aluOp === MIPS.MADD_Op) || (flowReg2.aluOp === MIPS.MSUB_Op))
                    & (flowReg2.reg1(31).asUInt === 1)) ? (~flowReg2.reg1 + 1) | flowReg2.reg1)

    opData2Mult := ((((flowReg2.aluOp === MIPS.MUL_Op) || (flowReg2.aluOp === MIPS.MULT_Op)
                    || (flowReg2.aluOp === MIPS.MADD_Op) || (flowReg2.aluOp === MIPS.MSUB_Op))
                    & (flowReg2.reg2(31).asUInt === 1)) ? (~flowReg2.reg2 + 1) | flowReg2.reg2)

    HILOTemp := opData1Mult * opData2Mult

    when((flowReg2.aluOp === MIPS.MULT_Op) || (flowReg2.aluOp === MIPS.MUL_Op)
      || (flowReg2.aluOp === MIPS.MADD_Op) || (flowReg2.aluOp === MIPS.MSUB_Op)){
      when((flowReg2.reg1(31) ^ flowReg2.reg2(31)).asUInt === 1){
        mulRes := ~HILOTemp + 1
      }.otherwise{
        mulRes := HILOTemp
      }
    }.otherwise{
      mulRes := HILOTemp
    }

    //------------------------------------
    //MADD,MADDU,MSUB,MSUBU指令
    switch(flowReg2.aluOp){
      is(MIPS.MADD_Op,MIPS.MADDU_Op){
        when(cntIn === 0){
          HILOTempOut  := HISwitch @@ LOSwitch
          HILOTemp1    := mulRes
          cntOut       := 1
          stallReq4MOp := 1
        }.elsewhen(cntIn === 1){
          HILOTempOut  := HILOTempIn + (HISwitch @@ LOSwitch)
          HILOTemp1    := 0
          cntOut       := 0
          stallReq4MOp := 0
        }.otherwise{
          HILOTemp1    := 0
          HILOTempOut  := 0
          cntOut       := 0
          stallReq4MOp := 0
        }
      }
      is(MIPS.MSUB_Op,MIPS.MSUBU_Op){
        when(cntIn === 0){
          HILOTempOut  := HISwitch @@ LOSwitch
          HILOTemp1    := ~mulRes + 1
          cntOut       := 1
          stallReq4MOp := 1
        }.elsewhen(cntIn === 1){
          HILOTempOut  := HILOTempIn +(HISwitch @@ LOSwitch)
          HILOTemp1    := 0
          cntOut       := 0
          stallReq4MOp := 0
        }.otherwise{
          HILOTemp1    := 0
          HILOTempOut  := 0
          cntOut       := 0
          stallReq4MOp := 0
        }
      }
      default{
        HILOTempOut  := 0
        cntOut       := 0
        stallReq4MOp := 0
        HILOTemp1    := 0
      }
    }
    //------------------------------------
    //satllReq选择
    stallReq := stallReq4MOp
    flowCtrl.stallFromEX := stallReq
    //------------------------------------
    //确定要写入目的寄存器的值
    when(((flowReg2.aluOp === MIPS.ADD_Op) || (flowReg2.aluOp === MIPS.ADDI_Op) ||
      (flowReg2.aluOp === MIPS.SUB_Op)) && (ovSum === 1)){
      wReg := 0
    }.otherwise{
      wReg := flowReg2.wReg
    }
    //------------------------------------
    //数据前推选择，具有优先级
    when(memWdHILO === 1){
      HISwitch := memHI
      LOSwitch := memLO
    }.elsewhen(wbWdHILO === 1){
      HISwitch := wbHI
      LOSwitch := wbLO
    }.otherwise{
      HISwitch := HIData
      LOSwitch := LOData
    }
    //------------------------------------
    //读HI/LO寄存器的指令
    switch(flowReg2.aluOp){
      is(MIPS.MFHI_Op){
        moveRes := HISwitch
      }
      is(MIPS.MFLO_Op){
        moveRes := LOSwitch
      }
      is(MIPS.MOVZ_Op){
        moveRes := flowReg2.reg1
      }
      is(MIPS.MOVN_Op){
        moveRes := flowReg2.reg1
      }
      default{
        moveRes := 0
      }
    }

    //------------------------------------
    //写HI/LO寄存器的指令
    when((flowReg2.aluOp === MIPS.MULT_Op) || (flowReg2.aluOp === MIPS.MULTU_Op)){
      wdHILO := 1
      HI     := mulRes(63 downto 32)
      LO     := mulRes(31 downto 0)
    }.elsewhen((flowReg2.aluOp === MIPS.MADD_Op) || (flowReg2.aluOp === MIPS.MADDU_Op)){
      wdHILO := 1
      HI := HILOTempOut(63 downto 32)
      LO := HILOTempOut(31 downto 0)
    }.elsewhen((flowReg2.aluOp === MIPS.MSUB_Op) || (flowReg2.aluOp === MIPS.MSUBU_Op)){
      wdHILO := 1
      HI := HILOTempOut(63 downto 32)
      LO := HILOTempOut(31 downto 0)
    }.elsewhen(flowReg2.aluOp === MIPS.MTHI_Op){
      wdHILO := 1
      HI     := flowReg2.reg1
      LO     := LOSwitch
    }.elsewhen(flowReg2.aluOp === MIPS.MTLO_Op){
      wdHILO := 1
      HI     := HISwitch
      LO     := flowReg2.reg1
    }.otherwise{
      wdHILO := 0
      HI     := 0
      LO     := 0
    }

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
        wData := logicOut
      }
      is(MIPS.RES_Shift){
        wData := shiftOut
      }
      is(MIPS.RES_MOVE){
        wData := moveRes
      }
      is(MIPS.RES_ARITHMETIC){
        wData := armthmeticres
      }
      is(MIPS.RES_MUL){
        wData := mulRes(31 downto 0)
      }
      default{
        wData := 0
      }
    }
  }
//------------------------------------
//三级流水线寄存器
  val flowReg3 = new Area{
    val stall  = flowCtrl.stall

    val wd        = Reg(UInt(5 bits))init(0)
    val wReg      = Reg(UInt(1 bits))init(0)
    val wData     = Reg(UInt(instWidth bits))init(0)

    val HILOTemp1 = Reg(UInt(2*instWidth bits))init(0)
    val cnt       = Reg(UInt(2 bits))init(0)

    val wdHILO    = Reg(UInt(1 bits))init(0)
    val HI        = Reg(UInt(instWidth bits))init(0)
    val LO        = Reg(UInt(instWidth bits))init(0)

    when((stall(3) === True)){
      //KeepValue
    }.elsewhen(stall(3) === False){
      wd        := EX.wd
      wReg      := EX.wReg
      wData     := EX.wData
      wdHILO    := EX.wdHILO
      HI        := EX.HI
      LO        := EX.LO
      HILOTemp1 := EX.HILOTemp1
      cnt       := EX.cntOut
    }.otherwise{
      wd        := 0
      wReg      := 0
      wData     := 0
      wdHILO    := 0
      HI        := 0
      LO        := 0
      HILOTemp1 := 0
      cnt       := 0
    }

    EX.cntIn := cnt
    EX.HILOTempIn := HILOTemp1
  }
//------------------------------------
//MEM模块
  val MEM = new Area{
    val wd     = flowReg3.wd
    val wReg   = flowReg3.wReg
    val wData  = flowReg3.wData

    val wdHILO = flowReg3.wdHILO
    val HI     = flowReg3.HI
    val LO     = flowReg3.LO

    ID.memWData  := wData
    ID.memWReg   := wReg
    ID.memWd     := wd

    EX.memWdHILO := wdHILO
    EX.memHI     := HI
    EX.memLO     := LO
  }
//------------------------------------
//四级流水线寄存器
  val flowReg4 = new Area{
    val stall  = flowCtrl.stall

    val wd     = Reg(UInt(5 bits))init(0)
    val wReg   = Reg(UInt(1 bits))init(0)
    val wData  = Reg(UInt(instWidth bits))init(0)

    val wdHILO = Reg(UInt(1 bits))init(0)
    val HI     = Reg(UInt(instWidth bits))init(0)
    val LO     = Reg(UInt(instWidth bits))init(0)

    when((stall(4) === True)){
      //KeepValue
    }.elsewhen(stall(4) === False){
      wd     := MEM.wd
      wReg   := MEM.wReg
      wData  := MEM.wData
      wdHILO := MEM.wdHILO
      HI     := MEM.HI
      LO     := MEM.LO
    }.otherwise{
      wd     := 0
      wReg   := 0
      wData  := 0
      wdHILO := 0
      HI     := 0
      LO     := 0
    }

    EX.wbWdHILO := wdHILO
    EX.wbHI     := HI
    EX.wbLO     := LO
  }

//------------------------------------
//HI/LO寄存器

  val HILO = new Area{
    val HIReg = Reg(UInt(32 bits))init(0)
    val LOReg = Reg(UInt(32 bits))init(0)

    EX.HIData := HIReg
    EX.LOData := LOReg

    when(flowReg4.wdHILO === 1){
      HIReg := flowReg4.HI
      LOReg := flowReg4.LO
    }
  }

//------------------------------------
//通用寄存器模块
  val regFiles = new Area{
    val regs = Mem(UInt(32 bits),32)
    //------------------------------------
    //写操作
    when((flowReg4.wReg === 1) && (flowReg4.wd =/= 0)){
      regs(flowReg4.wd) := flowReg4.wData
    }
    //------------------------------------
    //端口1读操作
    when(ID.reg1Addr === 0){
      ID.reg1Data := 0
    }.elsewhen((ID.reg1Addr === flowReg4.wd) && (flowReg4.wReg === 1) && (ID.reg1Read === 1)){
      ID.reg1Data := flowReg4.wData
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
      ID.reg2Data := flowReg4.wData
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
    val ce   = in UInt(1 bits)
    val inst = out UInt(instWidth bits)
  }
  //------------------------------------
  //加载ROM初始化数据
  val patch   = "C:\\Users\\ZLXT\\Desktop\\SpinalTemplateSbt\\src\\main\\scala\\mylib\\rom.data"
  val source  = new loadData
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
  myRom.io.ce   := myCpu.io.romCe
  myRom.io.addr := myCpu.io.romAddr
  myCpu.io.inst := myRom.io.inst
}


object MyTopLevelVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new MyTopLevel)
  }
}