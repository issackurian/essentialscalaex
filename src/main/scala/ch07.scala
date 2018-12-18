package ch07

object ex_7_1_6_1 {
  def run: Unit = {
    val absOrdering = new Ordering[Int] {
      def compare(x: Int, y: Int): Int =
        Math.abs(x) - Math.abs(y)
    }

    assert(List(- 4 , - 1 , 0 , 2 , 3 ).sorted(absOrdering) == List( 0 , - 1 , 2 , 3 , - 4 ))
    assert(List(- 4 , - 3 , - 2 , - 1 ).sorted(absOrdering) == List(- 1 , - 2 , - 3 , - 4 ))

    implicit val implAbdOrdering = new Ordering[Int] {
      def compare(x: Int, y: Int): Int =
        Math.abs(x) - Math.abs(y)
    }

    assert(List(- 4 , - 1 , 0 , 2 , 3 ).sorted == List( 0 , - 1 , 2 , 3 , - 4 ))
    assert(List(- 4 , - 3 , - 2 , - 1 ).sorted == List(- 1 , - 2 , - 3 , - 4 ))
  }
}

object ex_7_1_6_2 {
  final case class Rational(numerator: Int, denominator: Int)

  implicit val rationalOrdering = new Ordering[Rational] {
    def compare(x: Rational, y: Rational): Int = {
      (x.numerator * y.denominator) - (y.numerator * x.denominator)
    }
  }

  def run: Unit =
    assert (List( Rational ( 1 , 2 ), Rational ( 3 , 4 ), Rational ( 1 , 3 )). sorted == List( Rational ( 1 , 3 ), Rational ( 1 , 2 ), Rational ( 3 , 4 )))
}

object ex_7_2_5_1 {
  final case class Order(units: Int, unitPrice: Double) {
    val totalPrice: Double = units * unitPrice
  }

  object OrderTotalPriceOrdering {
    implicit val ordering = new Ordering[Order] {
      def compare(x: Order, y: Order): Int =
        ((x.totalPrice * 100) - (y.totalPrice * 100)).toInt
    }
  }

  object OrderUnitOrdering {
    implicit val ordering = new Ordering[Order] {
      def compare(x: Order, y: Order): Int =
        (x.units) - (y.units)
    }
  }

  object OrderUnitPriceOrdering {
    implicit val ordering = new Ordering[Order] {
      def compare(x: Order, y: Order): Int =
        ((x.unitsPrice * 100) - (y.unitPrice * 100)).toInt
    }
  }
}

object ex_7_3_4_1 {
  case class Person(name: String, email: String)

  trait Equal[A] {
    def equal(x: A, y: A): Boolean
  }

  object PersonEmailEquality {
    implicit val eqInst = new Equal[Person] {
      def equal(x: Person, y: Person): Boolean =
        x.email == y.email
    }
  }

  object PersonFullEquality {
    implicit val eqInst = new Equal[Person] {
      def equal(x: Person, y: Person): Boolean =
        x.name == y.name &&
          x.email == y.email
    }
  }
}


