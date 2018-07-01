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


package totalitarian.tests

import totalitarian._
import probation._
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
    }.assert(_ == Returns(fqt"totalitarian.ident.Disjunct[totalitarian.![Int]]"))

    test("construction of an identity String disjunct") {
      scalac"""ident.Disjunct("something")"""
    }.assert(_ == Returns(fqt"totalitarian.ident.Disjunct[totalitarian.![String]]"))

    test("correctly infer type of list of disjuncts") {
      scalac"""List(ident.Disjunct(""), ident.Disjunct(0)).head"""
    }.assert(
      _ == Returns(
        fqt"totalitarian.ident.Disjunct[totalitarian.![Int] with totalitarian.![String]]"
      )
    )

    val twos = List(ident.Disjunct(2), ident.Disjunct("two!"))
    test("map over disjunct to same types") {
      import ident._
      List(Disjunct(1), Disjunct("two")).map { xs =>
        xs.map(
          on[Int] map { i =>
            i + 1
          },
          on[String] map { s =>
            s + "!"
          }
        )
      }
    }.assert(_ == twos)

    val threeFour = List(ident.Disjunct('three), ident.Disjunct(4.0))
    test("map over disjunct to different types") {
      import ident._
      List(Disjunct(1), Disjunct("two")).map { xs =>
        xs.map(
          on[Int] map { i =>
            'three
          },
          on[String] map { s =>
            4.0
          }
        )
      }
    }.assert(_ == List(ident.Disjunct('three), ident.Disjunct(4.0)),
             _ => "the result was not as expected")

    test("unification of types under mapping") {
      scalac"""
        import ident._
        List(Disjunct(1), Disjunct("")).head.map(
          on[Int] map { i => "int" },
          on[String] map { s => "string" }
        )
      """
    }.assert(_ == Returns(Fqt("String")))

    ()
  }
}
