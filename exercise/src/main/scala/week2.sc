//def sum(f: Int => Int, a: Int, b:Int): Int = {
//  @scala.annotation.tailrec
//  def loop(a: Int, acc: Int): Int = {
//    if (a > b) acc
//    else loop(a + 1, acc + f(a))
//  }
//
//  loop(a, 0)
//}

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, acc: Int)(a: Int, b: Int): Int =
  if (a > b) acc
  else combine(f(a), mapReduce(f, combine, acc)(a + 1, b))

def product(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

def sum(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x + y, 0)(a, b)

def fact(n: Int) = product(x => x)(1, n)

val tolerance = 0.0001


def isCloseEnough(x: Double, y: Double) =
  Math.abs(( x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1)

sqrt(2)

