package aaa.bbb

import java.util.Date

object CoverageEx {
  
  def mapCase() = {
    val l = List(1,get2(),3)
    //val l = List(List(1,9),List(2,9),List(3,9),List(4,9))
    //l.map{case x :: xs => x}
  }
  
  def get2():Int = 2
}