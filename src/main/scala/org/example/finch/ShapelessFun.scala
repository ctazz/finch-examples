package org.example.finch

object ShapelessFun {

  {
    case class Employee(name: String, number: Int, manager: Boolean)
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    //Need different csv functions even though these objects have the same fields
    def employeeCsv(e: Employee): List[String] = List(e.name, e.number.toString, e.manager.toString)
    def iceCreamCsv(c: IceCream): List[String] = List(c.name, c.numCherries.toString, c.inCone.toString)

    import shapeless._
    val genericEmployee = Generic[Employee].to(Employee("Dave", 123, false ))
    val genericIceCream = Generic[IceCream].to(IceCream("Sundae", 1, false ))

    def genericCsv(gen: String :: Int :: Boolean :: HNil): List[String] = List(gen(0), gen(1).toString, gen(2).toString)


    assert(genericCsv(genericEmployee) == List("Dave", "123", "false"))
    assert(genericCsv(genericIceCream) == List("Sundae", "1", "false"))
  }

  {
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends
      Shape
    final case class Circle(radius: Double) extends Shape
    val rect: Shape = Rectangle(3.0, 4.0)
    val circ: Shape = Circle(1.0)

    def area(shape: Shape): Double =
      shape match {
        case Rectangle(w, h) => w * h
        case Circle(r)       => math.Pi * r * r
      }
    assert(area(rect) == 12.0)

  }

  {
    //Could use Tuples and Eithers to represent Abstract Data Types that we usually represent with case classes and sealed traits
    //BUT note, we can't iterate through a tuple!
    type Rectangle2 = (Double, Double)
    type Circle2    = Double
    type Shape2     = Either[Rectangle2, Circle2]
    val rect2: Shape2 = Left((3.0, 4.0))
    val circ2: Shape2 = Right(1.0)

    def area2(shape: Shape2): Double =
      shape match {
        case Left((w, h)) => w * h
        case Right(r)     => math.Pi * r * r
      }


    assert(area2(rect2) == 12.0)
    // res4: Double = 12.0
    assert(area2(circ2) == math.Pi * circ2.right.get)

  }

  {
    import shapeless.Generic
    import shapeless._
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    val iceCreamGen: Generic[IceCream] = Generic[IceCream]

    val iceCream = IceCream("Sundae", 1, false)

    //repr is not an HList!
    val repr: iceCreamGen.Repr = iceCreamGen.to(iceCream)

    //But we can get an HList from repr
    val hListFromRepr: String :: Int :: Boolean :: HNil  = repr.asInstanceOf[String :: Int :: Boolean :: HNil]
    assert(hListFromRepr == "Sundae" :: 1 :: false :: HNil)

    val roundTrip: IceCream = iceCreamGen.from(repr)
    assert(roundTrip == iceCream)

    //This has the same shape as IceCream, and Generic will let  us turn an IceCream into an Employee
    case class Employee(name: String, number: Int, manager: Boolean)

    val employeeFromIceCream: Employee = Generic[Employee].from(
      Generic[IceCream].to(iceCream)
    )

    assert(employeeFromIceCream == Employee("Sundae", 1, false))

  }

  {
    import shapeless.Generic
    import shapeless._

    //Can do the same thing with tuples
    //So now you could iterate over tuples (as soon as we learn to iterate over HLists!)
    val tupleGen = Generic[(String, Int, Boolean)]
    val tupleRepr: tupleGen.Repr = tupleGen.to(("Hello", 123, true))
    // res4: tupleGen.Repr = Hello :: 123 :: true :: HNil
    val (str, theInt, theBool) : (String, Int, Boolean) = tupleGen.from(tupleRepr)
    assert( str == "Hello" && theInt == 123 && theBool)

  }

}
