package patmat

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  val nums = List(1, 3, 4, 5, 8);System.out.println("""nums  : List[Int] = """ + $show(nums ));$skip(30); 

  def f = (x: Int) => x == 4;System.out.println("""f: => Int => Boolean""");$skip(21); val res$0 = 
  nums dropWhile (f);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(22); val res$1 = 
  
  nums takeWhile f;System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(16); val res$2 = 

	nums filter f;System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(15); 
	
	val a = Nil;System.out.println("""a  : scala.collection.immutable.Nil.type = """ + $show(a ))}
}
