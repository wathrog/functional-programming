package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(2), 1))
  val unionSet = union(singletonSet(1), singletonSet(2))
  val filteredSet = filter(unionSet, (x : Int) => x <= 3);
  println(contains(filteredSet, 3))
  println(forall(unionSet, (x : Int) => x <= 1))
  println(exists(unionSet, (x : Int) => x > 2))
}
