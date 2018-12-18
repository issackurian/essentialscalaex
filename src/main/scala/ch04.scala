object Ch04 {
  sealed trait Feline {
    def color: String
    def sound: String = "Roar"
  }

  // TODO - the default impl "roar" for sound should be in a separate Trait - BigCat
  final case class Tiger(color: String) extends Feline 
  final case class Panther(color: String)
  final case class Lion(color: String, maneSize: Int) extends Feline
  final case class Cat(color: String, favouriteFood: String) extends Feline {
    override val sound = "Meow"
  }


  sealed trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }

  final case class Circle(radius: Double) extends Shape {
    override val sides: Int = 1
    override lazy val perimeter: Double = 2 * math.Pi * radius
    override lazy val area: Double = math.Pi * radius * radius
  }

  sealed trait Rectangular extends Shape {
    def length: Double
    def breadth: Double

    override val sides: Int = 4
    override val perimeter: Double = 2 * (length + breadth)
    override val area: Double = length * breadth
  }
  final case class Square(side: Double) extends Rectangular {
    override val length: Double = side
    override val breadth: Double = side
  }

  final case class Rectangle(length: Double, breadth: Double) extends Rectangular

  object Draw {
    def apply(shape: Shape): String = shape match {
      case Circle(radius) => s"Circle with radius - $radius"
      case Rectangle(length, breadth) => s"Rectangle with length: $length, breadth: $breadth"
      case Square(side) => s"Square of side $side"
    }
  }

  sealed trait Color {
    def red: Int
    def green: Int
    def blue: Int
    def isLight: Boolean = (red + green + blue) / 300 > 50
  }

  case object Red extends Color {
    override val red = 100
    override val green = 0
    override val blue = 0
  }

  case object Yellow extends Color {
    override val red = 100
    override val green = 100
    override val blue = 0
  }

  case object Blue extends Color {
    override val red = 100
    override val green = 0
    override val blue = 100
  }

  final case class CustomColor(red: Int, green: Int, blue: Int, dark: Boolean) extends Color
}
