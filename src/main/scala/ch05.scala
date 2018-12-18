package ch05

object ex_5_1_3 {
  sealed trait LinkedList[T] {
    def length: Int = this match {
      case End() => 0
      case Pair(h, t) => 1 + t.length
    }

    def contains(x: T): Boolean = this match {
      case End() => false
      case Pair(h, t) => if (x == h) true else t.contains(x)
    }

    def apply(n: Int): Result[T] = (n, this) match {
      case (_, End()) => Failure(s"List does not have enough elements")
      case (0, Pair(h, _)) => Success(h)
      case (x, Pair(_, t)) => t.apply(x-1)
    }
  }
  final case class End[T]() extends LinkedList[T]
  final case class Pair[T](head: T, tail: LinkedList[T]) extends LinkedList[T]

  sealed trait Result[A]
  final case class Success[A](result: A) extends Result[A]
  final case class Failure[A](reason: String) extends Result[A]

  object LinkedListTests {
    def lengthTests = {
      val example = Pair( 1 , Pair( 2 , Pair( 3 , End ())))
      assert (example.length == 3 )
      assert (example.tail.length == 2 )
      assert ( End().length == 0 )

      println("Tests Passed")
    }

    def containsTests = {
      val example = Pair( 1 , Pair( 2 , Pair( 3 , End())))
      assert (example.contains(3) == true )
      assert (example.contains(4) == false )
      assert (End().contains(0) == false )
      // This should not compile // example.contains("not an Int")
      println("Tests Passed")
    }

    def applyTests = {
      val example = Pair( 1 , Pair( 2 , Pair( 3 , End())))
      assert (example(0) == Success(1) )
      assert (example(1) == Success(2) )
      assert (example(2) == Success(3) )
      assert (example(3) == Failure("List does not have enough elements"))
      println("Tests Passed")
    }
  }
}

