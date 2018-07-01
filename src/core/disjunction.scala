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

import scala.language.existentials
import scala.language.higherKinds
import scala.annotation.implicitNotFound
import scala.language.implicitConversions

import annotation.unchecked.{uncheckedVariance => uv}

import language.dynamics, language.experimental.macros
import scala.reflect.macros.whitebox

object TypeId {
  implicit def genTypeId[T]: TypeId[T] = macro TypeId.gen[T]

  def gen[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val typ = implicitly[WeakTypeTag[T]].tpe
    if (!typ.typeSymbol.isStatic)
      c.abort(c.enclosingPosition, s"cannot find TypeId for ${typ.toString}")
    q"new _root_.totalitarian.TypeId[${weakTypeOf[T]}](${typ.toString}.intern)"
  }

  implicit val intId: TypeId[Int] = TypeId("scala.Int")
  implicit val doubleId: TypeId[Double] = TypeId("scala.Double")
  implicit val charId: TypeId[Char] = TypeId("scala.Char")
  implicit val byteId: TypeId[Byte] = TypeId("scala.Byte")
  implicit val floatId: TypeId[Float] = TypeId("scala.Float")
  implicit val longId: TypeId[Long] = TypeId("scala.Long")
  implicit val shortId: TypeId[Short] = TypeId("scala.Short")
  implicit val booleanId: TypeId[Boolean] = TypeId("scala.Boolean")
}

trait Fallback
trait NoFallback extends Fallback

object `package` {

  def when[V](fn: V => Boolean): When[V, NoFallback] = When(fn)
  def when[V](value: V): When[V, NoFallback] = When(_ == value)
  def otherwise[V]: When[V, Fallback] = When(_ => true)

}

case class When[V, F](fn: V => Boolean) {
  def use[R: TypeId](v: R): MapValue[R, R, V, F] = new MapValue(fn, _ => v)
}

case class TypeId[T](private val tpe: String) extends AnyVal

/** intermediate factory object for creating a new [[MapClause]] */
class OfType[In, V, R <: Relation](val r: R) {

  /** constructs a new [[MapClause]] with the given `action` */
  def map[Return: TypeId, W](action: V => Return)(
    implicit ev: TypeId[In],
    key: r.Key[In, V],
    ev2: W =:= ![Return]
  ): MapClause[In, ![In], Return, Return, V, W, R] =
    new MapClause[In, ![In], Return, Return, V, W, R](action)(
      ev,
      implicitly[TypeId[Return]],
      ev2
    )

  def use[Return: TypeId, W](value: => Return)(
    implicit ev: TypeId[In],
    key: r.Key[In, V],
    ev2: W =:= ![Return]
  ): MapClause[In, ![In], Return, Return, V, W, R] = map[Return, W](_ => value)
}

case class TypeIndex[T](tag: TypeId[T]) {
  override def equals(that: Any): Boolean = that match {
    case that: TypeIndex[_] =>
      tag == that.tag
    case _ =>
      false
  }

  override def hashCode: Int = tag.hashCode
}

sealed trait Base

/** a handler for a single case of the disjunction */
case class MapClause[-T: TypeId, -T2, +R: TypeId, -Return, -V, -W, Path <: Relation](val action: V => R)(
  implicit ev: W =:= ![R]
) {
  val fromTypeTag: TypeId[_] = implicitly[TypeId[T]]
  val toTypeTag: TypeId[_] = implicitly[TypeId[R]]
}

case class MapValue[+R, -R2: TypeId, In, +F](val fn: In => Boolean, val parse: In => R) {
  val toTypeTag: TypeId[_] = implicitly[TypeId[R2]]
  def get(value: In): R = parse(value)
}

/** An invariant type wrapper */
trait ![T] extends Base

object Parser {
  def apply[Input] = new OfType[Input]()
  class OfType[Input]() {
    def apply[R, R2, V, W, F](cases: MapValue[R, R2, Input, F]*)(implicit hasFallback: Fallback =:= F, resultType: ident.Disjunct.ResultType[R, R2, Input]): Parser[Input, R, R2, V, W, resultType.type, F] =
      new Parser[Input, R, R2, V, W, resultType.type, F](cases, resultType)
  }
}

class Parser[Input, R, R2, V, W, RT <: ident.Disjunct.ResultType[R, R2, Input], F](cases: Seq[MapValue[R, R2, Input, F]], val resultType: RT) {
  def parse(value: Input) = {
    val r = cases.find(_.fn(value)).get
    resultType.wrap(r.get(value), r.toTypeTag)
  }
}

class Relation { relation =>

  case class Key[K, V]() {
    type Value = V
    type Key = K
  }
  
  /** companion object for [[Disjunct]] type, including factory methods */
  object Disjunct {

    /** intermediate class for the construction of new [[Disjunct]]s */
    case class OfType[Type]() {

      /** creates a new [[Disjunct]] of the value [[value]]
        *
        *  This method works best when the type is inferred.
        *
        *  @param  value  the value to be stored in the disjunction
        *  @tparam V      the inferred type of the disjunction
        *  @return        a new disjunction */
      def apply[K, V](value: V)(implicit key: Key[K, V],
                                evidence: Type <:< V,
                                typeId: TypeId[K]): Disjunct[![K]] =
        new Disjunct[![K]](value, typeId)
    }

    /** factory method for creating a new [[Disjunct]] of a specified type
      *
      *  @tparam Type  the type of the disjunction as an intersection type
      *  @return       an instance of the intermediate [[OfType]] class */
    def of[Type]: OfType[Type] = OfType()

    /** creates a new [[Disjunct]]
      *
      *  @param  value  the value to be stored in the disjunction
      *  @tparam Type   the intersection type of the disjunction inferred from the result
      *                 type
      *  @tparam T      the type of the disjunction, inferred from the value parameter
      *  @return        a new [[Disjunct]] instance */
    def apply[Type, K](value: Type)(implicit key: Key[K, Type],
                                    typeId: TypeId[K]): Disjunct[![K]] =
      new Disjunct(value, typeId)

    def apply[Type, K <: AnyRef](
      key: K,
      value: Type
    )(implicit ev: Key[K, Type], typeId: TypeId[key.type]): Disjunct[![key.type]] =
      new Disjunct[![key.type]](value, typeId)

    /** type which exists to resolve disjunctions of a single type to the raw type
      *
      *  An instance of this type will be resolved implicitly to one of two values
      *  (defined in the companion object), depending on the equality or inequality of the
      *  type arguments [[T1]] and [[T2]], which are typically inferred from repeated
      *  arguments, one covariantly and the other contravariantly. */
    trait ResultType[T1, T2, W] {
      type Wrap[T1, T2, W]
      def wrap(value: T1, typeId: TypeId[_]): Wrap[T1, T2, W]
    }

    /** companion object for the [[ResultType]] type, providing prioritized implicits */
    object ResultType extends ResultType_1 {

      /** the implicit to be resolved when the compared types are equal */
      implicit def equalTypes[T1, T2, W](implicit ev: T1 =:= T2): ResultType[T1, T2, W] {
        type Wrap[T1, T2, W] = T2
      } = new ResultType[T1, T2, W] {

        /** the result type, which, given equal types, can be either one of them */
        type Wrap[T1, T2, W] = T2

        /** the value-level function which returns a value conforming to the [[Wrap]] type
          */
        def wrap(value: T1, typeId: TypeId[_]): T2 = value
      }
    }

    /** trait to be mixed in to [[ResultType]]'s companion providing a lower-priority
      *  implicit */
    trait ResultType_1 {

      /** the fallback implicit to be resolved when the compared types are unequal */
      implicit def unequalTypes[T1, T2, W]
        : ResultType[T1, T2, W] { type Wrap[T1, T2, W] = ident.Disjunct[W] } =
        new ResultType[T1, T2, W] {

          /** result type which, for unequal types, means we return the second type */
          type Wrap[T1, T2, W] = ident.Disjunct[W]

          /** the value-level method which returns a value conforming to the [[Wrap]] type
            *
            *  For unequal types, which correspond to more than one type in the
            *  disjunction, we return an instance of [[Disjunct]].
            *
            *  @param value    the actual value wrapped by the disjunction
            *  @param typeId  the [[TypeId]] of the value's actual type */
          def wrap(value: T1, typeId: TypeId[_]): ident.Disjunct[W] =
            new ident.Disjunct(value, typeId)
        }
    }

    /** companion object for providing an [[AllCases]] instance if the type inequality
      *  holds */
    object AllCases {
      implicit def allCases[T1, T2](implicit ev: T1 <:< T2): AllCases[T1, T2] = AllCases()
    }

    /** dummy type which should be implicitly resolvable if a map-block correctly covers
      *  all cases */
    @implicitNotFound("not all cases have been specified")
    case class AllCases[T1, +T2]()

  }

  /** a disjunction, as a single value of several possible types */
  class Disjunct[-Type](val value: Any, val typeId: TypeId[_]) {

    override def equals(that: Any): Boolean = that match {
      case that: Disjunct[_] => that.typeId == typeId && that.value == value
      case _                 => false
    }

    override def hashCode: Int = typeId.hashCode ^ value.hashCode

    /** total function for handling all disjunction possibilities
      *
      *  This method takes repeated arguments, each handling a branch of the disjunction.
      *  The inferred type of the arguments is constrained such that every case of the
      *  disjunction must be handled. The return type will typically be a new [[Disjunct]]
      *  of the return types of each handled case, however, if every case returns the same
      *  type, the result type will the raw type.
      *
      *  The dependently-type return type uses the clever trick that the least upper-bound
      *  of a covariant parameter in a set of types will be the LUB of the parameter
      *  types, and the LUB of a contravariant parameter in the same set of types will be
      *  the intersection of those parameter types, and the covariant and contravariant
      *  parameter types will be the same if and only if the type of each argument is the
      *  same.
      *
      *  @param  cases  the handlers for each case of the disjunction
      *  @tparam T      the intersection type representing each branch of the disjunction
      *  @tparam R      the covariantly-inferred result type
      *  @tparam R2     the contravariantly-inferred result type
      *  @return        the dependently-typed value in a [[Disjunct]] or possibly
      *                 unwrapped */
    def map[T, T2 <: ![T], R, R2, V, W](cases: MapClause[T, T2, R, R2, V, W, relation.type]*)(
      implicit ev: Disjunct.AllCases[T2, Type],
      resultType: Disjunct.ResultType[R, R2, W]
    ): resultType.Wrap[R, R2, W] = {

      val thisCase = cases.find(_.fromTypeTag == typeId).get
      resultType.wrap(thisCase.action(value.asInstanceOf[V]), thisCase.toTypeTag)
    }

    def get[K: TypeId](implicit key: Key[K, _]): Option[key.Value] =
      if (implicitly[TypeId[K]] == typeId) Some(value.asInstanceOf[key.Value]) else None

    /** just delegate to the value's [[toString]] method */
    override def toString() = value.toString
  }

  /** construct an intermediate [[OfType]] instance for creating a [[MapClause]] */
  def on[T](implicit key: relation.Key[T, _]): OfType[T, key.Value, relation.type] =
    new OfType[T, key.Value, relation.type](relation)

  /** construct an intermediate [[OfType]] instance for creating a [[MapClause]] */
  def at[T <: Singleton](value: T)(implicit key: relation.Key[T, _]): OfType[T, key.Value, relation.type] =
    new OfType[T, key.Value, relation.type](relation)

  object Conjunct extends Dynamic {
    def apply[K, V](value: V)(implicit typeId: TypeId[K],
                              rel: Key[![K], V]): Conjunct[![K]] =
      new Conjunct[![K]](Map(TypeIndex(implicitly[TypeId[K]]) -> value))

    def of[K]: Construct[K] = new Construct[K]()

    class Construct[K]() {
      def apply[V](value: V)(implicit typeId: TypeId[K],
                             rel: Key[![K], V]): Conjunct[![K]] =
        Conjunct[K, V](value)
    }
  }

  class Conjunct[Type](val values: Map[TypeIndex[_], Any]) extends Dynamic {

    override def toString: String = values.map(_._2.toString).mkString("{", ", ", "}")

    def plus[K: TypeId, V](value: V)(
      implicit rel: Key[![K], V],
      ev: NotEqual[Type, Type with ![K]]
    ): Conjunct[Type with ![K]] =
      new Conjunct[Type with ![K]](
        values.updated(TypeIndex(implicitly[TypeId[K]]), value)
      )

    def and[K]: And[K] = new And[K]()

    class And[K]() {
      def apply[V](value: V)(
        implicit typeId: TypeId[K],
        rel: Key[![K], V],
        ev: NotEqual[Type, Type with ![K]]
      ): Conjunct[Type with ![K]] =
        new Conjunct[Type with ![K]](
          values.updated(TypeIndex(implicitly[TypeId[K]]), value)
        )
    }

    def merge[Ks: TypeId](that: Conjunct[Ks]): Conjunct[Type with Ks] =
      new Conjunct[Type with Ks](values ++ that.values)

    def apply[K]: Apply[K] = new Apply[K]()

    class Apply[K]() {
      def apply[V]()(implicit tt: TypeId[K], ev: Type <:< ![K], rel: Key[![K], V]): V = {
        println(s"apply $tt")
        println(s"in $values")
        values(TypeIndex(tt)).asInstanceOf[V]
      }
    }

    def subset[Ks](implicit tag: TypeId[Ks], ev: Type <:< Ks): Conjunct[Ks] =
      new Conjunct[Ks](values.filterKeys { k =>
        tag == k.tag
      })
  }

  trait NotEqual_1 {
    implicit def notEqual3[A, B] = NotEqual[A, B]()
  }

  object NotEqual extends NotEqual_1 {
    implicit def areEqual[A]: NotEqual[A, A] = NotEqual[A, A]()
    implicit def areEqual2[A]: NotEqual[A, A] = NotEqual[A, A]()
  }

  //@annotation.implicitAmbiguous("the conjunction already contains this type")
  case class NotEqual[A, B]()

}

object ident extends Relation {

  implicit def autoDisjunct[T: TypeId](value: T): Disjunct[![T]] = Disjunct(value)

  def from[T](value: T): Disjunct[Nothing] = macro Macros.coproduct[T]

  implicit def sameType[T]: Key[T, T] = Key[T, T]()
}
