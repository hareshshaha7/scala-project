import java.util.InputMismatchException

def removeAt(n: Int, list: List[Int]): List[Int] =
  if n < 0 then throw new InputMismatchException()
  else if n >= list.size then list
  else if n == 0 then list.tail
  else list match
    case Nil => Nil
    case x :: xs => x :: removeAt(n - 1, xs)

var list: List[Int] = List(1, 2, 3, 4, 5)
removeAt(-1, list)
removeAt(0, list)
removeAt(1, list)
removeAt(10, list)
removeAt(1, List())

def flatten(xs: Any): List[Any] =
  xs match
    case Nil => Nil
    case y :: ys => flatten(y) ::: flatten(ys)
    case _ => xs :: Nil


flatten(List(List(1, 1), 2, List(3, List(5, 8))))

val sampleList = List(0, 2, 4, 1, 3, 5, 6, 7, 8, 9)
sampleList.take(5)
sampleList.drop(5)
sampleList.slice(4, 8)

sampleList.map(x => x * x)
sampleList.filter(x => x % 2 == 0)
sampleList.filterNot(x => x % 2 == 0)
sampleList.partition(x => x % 2 == 0)

sampleList.takeWhile(x => x % 2 == 0)
sampleList.dropWhile(x => x % 2 == 0)
sampleList.span(x => x % 2 == 0)

def pack[T](ls: List[T]): List[List[T]] =
  ls match
    case Nil => Nil
    case y :: ys => val span = ls.span(x => x == y); span._1 :: pack(span._2)

pack("aaabcca".toList)

// Run length encoading
def encode[T](ls: List[T]): List[(T, Int)] =
  pack(ls).map(x=> (x.head, x.size))
//  ls match
//    case Nil => Nil
//    case y :: ys => val span = ls.span(x => x == y); (y, span._1.size) :: pack(span._2)


encode("aaabcca".toList)