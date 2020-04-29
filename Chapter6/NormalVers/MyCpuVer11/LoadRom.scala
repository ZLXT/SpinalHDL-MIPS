package mylib

import scala.util.Random
import scala.io.Source
import scala.math._

case class loadData(){
    def loadRomData(patch: String)={
        val source = Source.fromFile(patch,"UTF-8")
        val sourceData = source.getLines().toArray
        source.close()

        val romData = for(x <- sourceData)yield{
        var num:Long = 0
        var lineSize = x.length
        for(i <- x){
            val n = 
            if(i >= 'a'&& i <= 'f'){
                i - 'a' + 10
            }else{
                i - '0'
            }
            lineSize -= 1
            num = num + n*pow(16,lineSize).toLong
        }
        num
        }
        romData
    }
}