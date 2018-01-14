import org.apache.log4j.Logger

class Collatz {

  def input(myList: List[Int]): List[Int] = {

    print("Want to check for more \n 1 for more \n 2 to exit")
    val n = readInt()
    n match{
      case 1 => {
        print("Enter the number")
        val num=readInt()
        testing(myList,List(),num)
      }
      case _ => myList
    }
  }

  def testing(myList: List[Int],list: List[Int], num: Int): List[Int] = {
    def check(myList: List[Int], number: Int): Boolean = {
      myList match {
        case head :: tail if (head != number) => check(tail, num)
        case head :: tail if (head == number) => true
        case head :: Nil if (head != number) => false
        case head :: Nil if (head == number) => true
        case _ =>    false
      }
    }

    check(myList, num) match {
      case true => input(myList ::: list)
      case false => {
        num % 2 match {
          case 0 => testing( myList,num / 2 :: list ,num / 2)
          case 1 => testing( myList,3 * num + 1::list ,3 * num + 1)
        }
      }
    }
  }
}



object TestNumbers extends App {
  val log = Logger.getLogger(this.getClass)
  log.info("Enter your number")
  val n = readInt()
  val myList = List(1)
  val list=List()
  val obj = new Collatz
  log.info(obj.testing(myList,list,n))

}
