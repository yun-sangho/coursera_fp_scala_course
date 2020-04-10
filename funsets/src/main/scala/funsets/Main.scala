package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val a = singletonSet(10)
  val b = singletonSet(20)

  println(forall(a, b))
}
