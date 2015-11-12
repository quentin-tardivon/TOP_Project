object Main {
  def main(args: Array[String]) : Unit = {


def pow(m:Int,n:Int) :Int = n match {
	case 0 => return 1
	case _ => m*pow(m,n-1)
}

var value = 0x0300D03F
	
println(value)
println(value %16)
println((value % pow(16,2)))
println(value %pow(16,3))





  }
}
