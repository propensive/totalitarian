package totalitarian

import language.implicitConversions

trait Inv[T]

class Wrap[-T](t: Any)

case class Thing[T]() {
  def contains[S](implicit ev: T <:< Inv[S]) = 1
}

object Test {
  implicit def autoWrap[T](t: T): Wrap[Inv[T]] = new Wrap[Inv[T]](t)
  def infer[T](deps: Wrap[T]*): Thing[T] = Thing[T]
  val thing = infer(1, "two", new RuntimeException(), new Exception())
  thing.contains[Int]
  //thing.contains[Double]
  //thing.contains[Throwable]
  thing.contains[Exception]
  thing.contains[RuntimeException]
}
