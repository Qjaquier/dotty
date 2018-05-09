package aaa.bbb

import java.util.Date

object CoverageEx {

    val out = true
    /*
  class Person() {
    private var age_ = 0
    
    def age = age_
    
    def age_= (value:Int):Unit = age_ = value
  
  }*/

  def main(args: Array[String]) = {
  //var person = new Person()
   // print(person.age)
    //person.age = 10
     //val a = true && false
     val l = List(1,2)
    val l1 = l.filterNot(_%2 == 0).map(x =>               2*x)
     
     //val b = true || false && get() || true
  //val a = CoverageEx.select(CoverageEx.select(10))
    /*
    {val a = CoverageEx.select();a}
    CoverageEx.listM(List(1,2))
    tryB()
    CoverageEx.if1()
    if2()
    patMat(1234)
    val k : Int = 12 + 23
    val bb : Int => Boolean = x : Int => (x < 17 && (x +k) < k)
    def ddDef : Int => Boolean = x : Int => (x < 17 && (x +k) < k)
    val a = Some(1)
    val b : Option[Int] = Some(2)
    val c = None
    val d : Option[Int] = None
    select()
    mapCase()
    */
  }
  
  def get() : Boolean = {
    true
  }
  /*
  def mapCase() = {
  
    def3(true,get2() == 2,false)
    val l = List(1,get2(),3)
    //val l = List(List(1,9),List(2,9),List(3,9),List(4,9))
    //l.map{case x :: xs => x}
  }
  
  def get2():Int = 2
  
  def def3(a: Boolean, b: Boolean, c: Boolean):Boolean = {
    a && b && c
  }*/
  /*
    def tryB() = {
    try {
       
       
       CoverageEx.throwEx(CoverageEx.throwEx(CoverageEx.throwEx(false)))
    } catch {
      case foo: IllegalArgumentException => print("Exception catched !")
      case _: Throwable => println("Got some other kind of exception")
    } finally {
            print("Finally block")
    }
    
    try {
       throwEx(throwEx(throwEx(false)))
    } catch {
      case foo: IllegalArgumentException => print("Exception catched !")
      case _: Throwable => println("Got some other kind of exception")
    } finally {
            print("Finally block")
    }
  }
  
  def tryC() = {
     try {
       
       val shoulBeCovered = true
       val shoulNOTBeCovered = true
       def3(shoulBeCovered, throwEx(true), shoulNOTBeCovered) // def3 should not be covered
    } catch {
      case foo: IllegalArgumentException => print("Exception catched !")
      case _: Throwable => println("Got some other kind of exception")
    }
  }

  def throwEx(a : Boolean):Boolean = {
    if(a){
        true
        //throw new IllegalArgumentException("Argument is true !")
    } else {
        true
    }
  }
  
 
  
   
  private def bar: Int = 42
   
  def select(a : Int):Int = {
     CoverageEx.bar
     a
  }
   */
  /*
   def dat() = {
    val date = new Date()
    def nDate(): Date = new Date()
    print(date.getTime())
  }*/

   /*
    def listM(l : List[Int]):Unit = {
    l match {
        case x :: y :: z :: xs => print("Should not be called !")
        case x :: xs => print(x);listM(xs)
        case Nil => print("End of list !")
    }
  }*/

  /*

  

  
  def if1():Int = {
  	if(true) {
		print("trueMain")
	} else {
		print("falseMain")
	}
    return 1
  }
  def if2() = {
  	if(false) print("thenP") else print("elseP")
  }
  
  def patMat(asd : Int): Boolean = {
    val b = true
    val a = 12
    val k = asd
    val c = true || false && (true && false)
    
    if(c){
        val d = a + k
        print(d)
    } else {
        print("a")
    }
    a match {
        case 0 => if(false) print("thenP") else print("elseP")
        case 12 if (k == 1234) => print("12")
        case _ => print("Other")
    }
    
    return this.out
  }
  
  def tryB() = {
    try {
       CoverageEx.throwEx(CoverageEx.throwEx(CoverageEx.throwEx(false)))
    } catch {
      case foo: IllegalArgumentException => print("Exception catched !")
      case _: Throwable => println("Got some other kind of exception")
    } finally {
            print("Finally block")
    }
  }
  
  def throwEx(a : Boolean):Boolean = {
    if(a){
        throw new IllegalArgumentException("Argument is true !")
    } else {
        true
    }
  }
  */
  
}