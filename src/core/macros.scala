/*
  
  Totalitarian, version 1.0.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at
  
      http://www.apache.org/licenses/LICENSE-2.0
 
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License. */


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

    val subtypes: List[Type] =
      classType.knownDirectSubclasses.to[List].map(_.asType.toType)

    val intersectionType = subtypes.map(_.etaExpand) match {

      case Nil =>
        c.abort(c.enclosingPosition, "this type does not have any subclasses")

      case head :: tail =>
        tail.foldLeft(tq"$head") { (t1, t2) =>
          tq"$t1 with $t2"
        }
    }

    q"_root_.totalitarian.ident.Disjunct.of[$intersectionType]($value)"
  }
}
