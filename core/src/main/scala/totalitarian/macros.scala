package totalitarian

import scala.reflect._, macros._

object Macros {
  def coproduct[T: c.WeakTypeTag](c: whitebox.Context)(value: c.Tree): c.Tree = {
    import c.universe._
    
    val classType: ClassSymbol = weakTypeOf[T].typeSymbol.asClass match {
      
      // if the type is sealed, we're good
      case cls if cls.isSealed =>
        cls
      
      // if the type is not sealed, check if its parent is sealed, and use that if so
      case cls if cls.baseClasses.length > 1 && cls.baseClasses(1).asClass.isSealed =>
        cls.baseClasses(1).asClass
      
      // otherwise abort
      case _ =>
        c.abort(c.enclosingPosition, "this type is not sealed")
    }

    val subtypes: List[Type] = classType.knownDirectSubclasses.to[List].map(_.asType.toType)

    val intersectionType = subtypes.map(_.etaExpand) match {
      
      case Nil =>
        c.abort(c.enclosingPosition, "this type does not have any subclasses")
      
      case head :: tail =>
        tail.foldLeft(tq"$head") { (t1, t2) => tq"$t1 with $t2" }
    }

    q"_root_.totalitarian.ident.Disjunct.of[$intersectionType]($value)"
  }
}

