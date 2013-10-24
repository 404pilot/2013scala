package patmat

object test {
  val nums = List(1, 3, 4, 5, 8)                  //> nums  : List[Int] = List(1, 3, 4, 5, 8)

  def f = (x: Int) => x == 4                      //> f: => Int => Boolean
  nums dropWhile (f)                              //> res0: List[Int] = List(1, 3, 4, 5, 8)
  
  nums takeWhile f                                //> res1: List[Int] = List()

	nums filter f                             //> res2: List[Int] = List(4)
	
	val a = Nil                               //> a  : scala.collection.immutable.Nil.type = List()
}