package totalitarian.tests

import totalitarian._
import estrapade._
import contextual.data.scalac._
import contextual.data.fqt._
import contextual.data.txt._

object Cli extends Relation {
  implicit def key[P, T](implicit ev: P <:< Param[T]): Key[P, T] = Key[P, T]()
}

case class Param[T](name: String)

object Tests extends TestApp {
  
  object alpha extends Param[String]("alpha")
  object beta extends Param[Int]("beta")

  def tests(): Unit = {

    test("construction of an identity Int disjunct") {
      scalac"""ident.Disjunct(1)"""
    }.assert(_ == Returns(fqt"totalitarian.ident.Disjunct[totalitarian.~[Int]]"))

    test("construction of an identity String disjunct") {
      scalac"""ident.Disjunct("something")"""
    }.assert(_ == Returns(fqt"totalitarian.ident.Disjunct[totalitarian.~[String]]"))

    test("correctly infer type of list of disjuncts") {
      scalac"""List(ident.Disjunct(""), ident.Disjunct(0)).head"""
    }.assert(_ == Returns(fqt"totalitarian.ident.Disjunct[totalitarian.~[Int] with totalitarian.~[String]]"))

    val twos = List(ident.Disjunct(2), ident.Disjunct("two!"))
    test("map over disjunct to same types") {
      import ident._
      List(Disjunct(1), Disjunct("two")).map { xs =>
        xs.map(
          on[Int] as { i => i + 1 },
          on[String] as { s => s+"!" }
        )
      }
    }.assert(_ == twos)

    val threeFour = List(ident.Disjunct('three), ident.Disjunct(4.0))
    test("map over disjunct to different types") {
      import ident._
      List(Disjunct(1), Disjunct("two")).map { xs =>
        xs.map(
          on[Int] as { i => 'three },
          on[String] as { s => 4.0 }
        )
      }
    }.assert(_ == List(ident.Disjunct('three), ident.Disjunct(4.0)), _ => "the result was not as expected")

    test("unification of types under mapping") {
      scalac"""
        import ident._
        List(Disjunct(1), Disjunct("")).head.map(
          on[Int] as { i => "int" },
          on[String] as { s => "string" }
        )
      """
    }.assert(_ == Returns(Fqt("String")))

    import Cli._

    val xs = List(Disjunct(alpha, "one"), Disjunct(beta, 2))


    val y = xs.map(_.map(
      at(beta) as { x => x + 1 },
      at(alpha) as { y => y }
    ))
   
    import ident.sameType

    val z = y.map(_.map(
      ident.on[Int] as { x => x + 1 },
      ident.on[String] as { x => x + "!" }
    ))

    println(z)

  }
}
