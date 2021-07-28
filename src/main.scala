object main extends App {
  def map[A](data: List[A], function: A => A): List[A] = {
    data match {
      case head :: tail => function(head) :: map(tail,function)
      case Nil => Nil
    }
  }

  def flatten[A](data: List[A]): List[Any] = data match {
    case (head:List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
    case Nil => Nil
  }

  /*def reduce[A](function: (A, A) => A, data: List[A]): List[Int] = data match {
    case head :: head2 :: tail => function(head,head2) + reduce(function,tail)
    case Nil => Nil
  }*/

  def concat[A](data: List[A], data2: List[A]): List[A] = (data, data2) match {
    case (head :: tail,_) => head :: concat(tail, data2)
    case (Nil,head :: tail) => head :: concat(Nil, tail)
    case (Nil,Nil) => (Nil)
  }

  def filter[A](data: List[A], filterfun: A => Boolean): List[A] = data match {
    case head :: tail => if (filterfun(head)) {
      head :: filter(tail, filterfun)
    } else {
      filter(tail,filterfun)
    }
    case Nil => Nil
  }

  def foldLeft[A,B](default: B, data: List[A],function: (A,B) => B): B = data match {
    case head :: tail => foldLeft(function(head, default), tail, function)
    case Nil => default
  }

  def foldRight[A,B](default: B, data: List[A],function: (A,B) => B): B = data match {
    case head :: tail => function(head, foldRight(default,tail,function))
    case Nil => default
  }

  def flatMap[A,B](data: List[A], function: A => List[B]): List[B] = data match {
    case head :: tail => flatMap(function(head),function) ++ flatMap(tail,function)
    case Nil => Nil
  }


  val func = (a : Int) => a*2
  val func2 = (a : Int, b : Int) => a+b
  val func3 = (a : Int) => a % 2 == 0

  val func4 = (a: Int, b: Int) => {
    println(a + ", " + b)
    a + b
  }
  val func5 = (a: List[Int]) => a.map(z => z+z)

  val res = (1 until 10)
  val reslist = res.toList
  val res2 = (1 until 5)
  val res2list = res2.toList
  val res3list = List(2,5,3)

  println(concat(reslist, res2list))
  println(filter(reslist,func3))
  println(foldRight(0, res3list,func4))
  println(foldLeft(0,res3list,func4))

  val list2 = List(List(1,2,3),List(1,2,3))
  println(flatMap(list2, func5))

}
