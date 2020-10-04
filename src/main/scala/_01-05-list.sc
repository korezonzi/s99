import scala.{+:, ::}

//Parameter
val numSeq = Seq(1, 1, 2, 3, 5, 8)
val numSeqPalindrome = Seq(1,2,3,2,1)
val numSeq2 = Seq(1,1)
val numSeq3 = Seq(4,56)

//P01 (*) Find the last element of a list.
//Example:
//scala> last(List(1, 1, 2, 3, 5, 8))
//res0: Int = 8
numSeq.last

//Find the last but one element of a list.
//  Example:
//  scala> penultimate(List(1, 1, 2, 3, 5, 8))
//res0: Int = 5
numSeq.init.last

//P03 (*) Find the Kth element of a list.
//By convention, the first element in the list is element 0.
//Example:
//scala> nth(2, List(1, 1, 2, 3, 5, 8))
//res0: Int = 2
//numSeq.zipWithIndex
//numSeq.
numSeq.applyOrElse(10, (n: Int) => "else"+n)

//P04 (*) Find the number of elements of a list.
//Example:
//scala> length(List(1, 1, 2, 3, 5, 8))
//res0: Int = 6
numSeq.length

//P05 (*) Reverse a list.
//Example:
//scala> reverse(List(1, 1, 2, 3, 5, 8))
//res0: List[Int] = List(8, 5, 3, 2, 1, 1)
//1
numSeq.reverse

//2
import scala.annotation.tailrec
def reverse[T](list: List[T]): List[T] = {
  @tailrec
  def go(rem: List[T], acc: List[T]): List[T] = rem match {
    case Nil => acc
    case x :: xs => go(xs, x :: acc)
  }
  go(list, Nil)
}

//3
numSeq.foldLeft(List.empty[Int]){(acc, n) =>
  n :: acc
}
