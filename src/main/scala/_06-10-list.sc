import scala.{+:, ::}

//Parameter
val numSeq = Seq(1, 1, 2, 3, 5, 8)
val numSeqPalindrome = Seq(1,2,3,2,1)
val numSeq2 = Seq(1,1)
val numSeq3 = Seq(4,56)

//P06 (*) Find out whether a list is a palindrome.
//Example:
//scala> isPalindrome(List(1, 2, 3, 2, 1))
//res0: Boolean = true
if(numSeq == numSeq.reverse) true else false
if(numSeqPalindrome == numSeqPalindrome.reverse) true else false

//P07 (**) Flatten a nested list structure.
//Example:
//scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//res0: List[Any] = List(1, 1, 2, 3, 5, 8)
val lists = List(List(1, 1), 2, List(3, List(5, 8)))
//Listがない -> Listつける -> flatten -> 繰り返す
//再起ver
def toSeq(lists: List[Any]): List[Any] = {
  lists.flatMap{
    case list: List[_] => toSeq(list)
    case num           => List(num)
  }
}
toSeq(lists)

//P08 (**) Eliminate consecutive duplicates of list elements.
//If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
//Example:
//
//scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).distinct

//P09 (**) Pack consecutive duplicates of list elements into sublists.
//If a list contains repeated elements they should be placed in separate sublists.
//Example:
//
//scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
//TODO: check
def pack[T](list: List[T]): List[List[T]]= {
  if(list.isEmpty) List(List.empty)
  else {
    //List(a,a), List(b,c,d) みたいにできる
    val (packed, next) = list.span(_ == list.head)
    //println(s"packed: ${packed}, next: ${next}")
    //packed: List(a,b,c,d), next() <- 最後まで処理終わった時
    if(next == Seq.empty) {
      //終わった答えを返す
      List(packed)
    }
    else {
      //再起: packedは既にカテゴライズした後に、残りの部分(next)だけを使う
      packed :: pack(next)
    }
  }
}

def pack2[T](ls: List[T]): List[List[T]] = {
  if(ls.isEmpty) List.empty
  else {
    val (pack, next) = ls.span(_ == ls.head)
    if(next.isEmpty) {
      List(pack)
    } else {
      pack :: pack2(next)
    }
  }
}
val charLs = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
pack(charLs)
pack2(charLs)

//P10 (*) Run-length encoding of a list.
//Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
//Example:
//
//scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
def encode(ls: List[Any]) =  {
  pack2(ls).map(x => (x.length, x.headOption.getOrElse("error")))
}
encode(charLs)
//P11 (*) Modified run-length encoding.
//Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
//Example:
//scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
def encodeModified(ls: List[Any]) = {
  pack2(ls).map{
    case rem if(rem.length <2 ) =>  rem.headOption.getOrElse("error")
    case x                      => (x.length, x.headOption.getOrElse("error"))
  }
}
encodeModified(charLs)

def encodeModified2(ls: List[Any]) = {
  encode(ls).map{t =>
    if(t._1 == 1) t._2
    else t
  }
}

def encodeModified3(ls: List[Any]): List[Either[Any, (Int, Any)]] = {
  encode(ls).map{t =>
    if(t._1 == 1) Left(t._2)
    else Right(t)
  }
}
encodeModified2(charLs)
encodeModified3(charLs)

//P12 (**) Decode a run-length encoded list.
//Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
//Example:
//
//scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
//res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
//encode(charLs)
encode(charLs).flatMap { x =>
  for {
    n <- 1 to x._1
  } yield {
    x._2
  }
}

//P13 (**) Run-length encoding of a list (direct solution).
//Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
//Example:
//
//scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))


//P14 (*) Duplicate the elements of a list.
//Example:
//scala> duplicate(List('a, 'b, 'c, 'c, 'd))
//res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
//P15 (**) Duplicate the elements of a list a given number of times.
//Example:
//scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)