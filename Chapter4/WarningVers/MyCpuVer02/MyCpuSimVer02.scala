package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.math._
//import scala.util.Random


//MyTopLevel's testbench
object MyTopLevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new MyTopLevel){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
      val source = new loadData
      val patch = "C:\\Users\\ZLXT\\Desktop\\SpinalTemplateSbt-master\\src\\main\\scala\\mylib\\rom.data"
      val romData = source.loadRomData(patch) 

      val regSize = pow(2,8).toInt
      val pipelineSize = 1
      for (idx <- 0 to pipelineSize){
        dut.clockDomain.waitRisingEdge()
      }
      
      for(idx <- 0 until regSize){
        //Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()
        /*
        if(idx < romData.size){
          //Check that the dut values match with the reference model ones
          assert(dut.io.inst.toLong == romData(idx))
        }else{
          //Check that the dut values match with the reference model ones
          assert(dut.io.inst.toLong == 0 )          
        }
        */
      }
      
    }
  }
}
