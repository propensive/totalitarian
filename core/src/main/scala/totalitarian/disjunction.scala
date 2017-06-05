package totalitarian

import scala.reflect.runtime.universe.TypeTag
import scala.language.existentials
import scala.language.higherKinds
import scala.annotation.implicitNotFound
import scala.language.implicitConversions

import annotation.unchecked.{uncheckedVariance => uv}

import language.dynamics, language.experimental.macros

case class TypeIndex[T](tag: TypeTag[T]) {
  override def equals(that: Any): Boolean = that match {
    case that: TypeIndex[_] =>
      tag.tpe =:= that.tag.tpe
    case _ =>
      false
  }

  override def hashCode: Int = tag.tpe.hashCode
}

class Relation {

  case class Keying[K, V]()

  /** companion object for [[Disjunct]] type, including factory methods */
  object Disjunct {

    /** intermediate class for the construction of new [[Disjunct]]s */
    final case class OfType[Type]() {

      /** creates a new [[Disjunct]] of the value [[value]]
       *
       *  This method works best when the type is inferred.
       *
       *  @param  value  the value to be stored in the disjunction
       *  @tparam V      the inferred type of the disjunction
       *  @return        a new disjunction */
      def apply[V](value: V)(implicit evidence: Type <:< V, typeTag: TypeTag[V]): Disjunct[Type] =
        new Disjunct(value, typeTag)
    }
    
    /** factory method for creating a new [[Disjunct]] of a specified type
     *
     *  @tparam Type  the type of the disjunction as an intersection type
     *  @return       an instance of the intermediate [[OfType]] class */
    def of[Type]: OfType[Type] = OfType()
    
    /** creates a new [[Disjunct]]
     *
     *  @param  value  the value to be stored in the disjunction
     *  @tparam Type   the intersection type of the disjunction inferred from the result type
     *  @tparam T      the type of the disjunction, inferred from the value parameter
     *  @return        a new [[Disjunct]] instance */
    def apply[Type, T <: Type](value: T)(implicit typeTag: TypeTag[T],
                                                  typeEquality: Type =:= T): Disjunct[Type] =
      new Disjunct(value, typeTag)

    /** type which exists to resolve disjunctions of a single type to the raw type
     *
     *  An instance of this type will be resolved implicitly to one of two values (defined in the
     *  companion object), depending on the equality or inequality of the type arguments [[T1]] and
     *  [[T2]], which are typically inferred from repeated arguments, one covariantly and the other
     *  contravariantly. */
    trait ResultType[T1, T2] {
      type Wrap[T1, T2]
      def wrap(value: T1, typeTag: TypeTag[_]): Wrap[T1, T2]
    }

    /** companion object for the [[ResultType]] type, providing prioritized implicits */
    object ResultType extends ResultType_1 {
      
      /** the implicit to be resolved when the compared types are equal */
      implicit def equalTypes[T1, T2](implicit ev: T1 =:= T2): ResultType[T1, T2] {
          type Wrap[T1, T2] = T2 } = new ResultType[T1, T2] {
        
        /** the result type, which, given equal types, can be either one of them */
        type Wrap[T1, T2] = T2
      
        /** the value-level function which returns a value conforming to the [[Wrap]] type */
        def wrap(value: T1, typeTag: TypeTag[_]): T2 = value
      }
    }

    /** trait to be mixed in to [[ResultType]]'s companion providing a lower-priority implicit */
    trait ResultType_1 {
      
      /** the fallback implicit to be resolved when the compared types are unequal */
      implicit def unequalTypes[T1, T2]: ResultType[T1, T2] { type Wrap[T1, T2] = Disjunct[T2] } =
        new ResultType[T1, T2] {
          /** result type which, for unequal types, means we return the second type */
          type Wrap[T1, T2] = Disjunct[T2]

          /** the value-level method which returns a value conforming to the [[Wrap]] type
           *
           *  For unequal types, which correspond to more than one type in the disjunction, we return
           *  an instance of [[Disjunct]].
           *
           *  @param value    the actual value wrapped by the disjunction
           *  @param typeTag  the [[TypeTag]] of the value's actual type */
          def wrap(value: T1, typeTag: TypeTag[_]): Disjunct[T2] =
            new Disjunct(value, typeTag)
        }
    }

    /** a handler for a single case of the disjunction */
    case class WhenClause[-T: TypeTag, +R, -Return: TypeTag](val action: T => R) {
      val fromTypeTag: TypeTag[_] = implicitly[TypeTag[T]]
      val toTypeTag: TypeTag[_] = implicitly[TypeTag[Return]]
    }

    /** companion object for providing an [[AllCases]] instance if the type inequality holds */
    object AllCases {
      implicit def allCases[T1, T2](implicit ev: T1 <:< T2): AllCases[T1, T2] = AllCases()
    }

    /** dummy type which should be implicitly resolvable if a when-block correctly covers all cases */
    @implicitNotFound("not all cases have been specified")
    case class AllCases[T1, +T2]()

  }

  /** a disjunction, as a single value of several possible types */
  class Disjunct[-Type](val value: T forSome { type T >: Type }, val typeTag: TypeTag[_]) {
    
    /** total function for handling all disjunction possibilities
     *
     *  This method takes repeated arguments, each handling a branch of the disjunction. The inferred
     *  type of the arguments is constrained such that every case of the disjunction must be handled.
     *  The return type will typically be a new [[Disjunct]] of the return types of each handled case,
     *  however, if every case returns the same type, the result type will the raw type.
     *
     *  The dependently-type return type uses the clever trick that the least upper-bound of a
     *  covariant parameter in a set of types will be the LUB of the parameter types, and the LUB of
     *  a contravariant parameter in the same set of types will be the intersection of those parameter
     *  types, and the covariant and contravariant parameter types will be the same if and only if
     *  the type of each argument is the same.
     *
     *  @param  cases  the handlers for each case of the disjunction
     *  @tparam T      the intersection type representing each branch of the disjunction
     *  @tparam R      the covariantly-inferred result type
     *  @tparam R2     the contravariantly-inferred result type
     *  @return        the dependently-typed value in a [[Disjunct]] or possibly unwrapped */
    def when[T, R, R2](cases: Disjunct.WhenClause[T, R, R2]*)(implicit ev: Disjunct.AllCases[T, Type],
        resultType: Disjunct.ResultType[R, R2]): resultType.Wrap[R, R2] = {
      
      val thisCase = cases.find(_.fromTypeTag.tpe <:< typeTag.tpe).get
      resultType.wrap(thisCase.action(value.asInstanceOf[T]), thisCase.toTypeTag)
    }

    /** just delegate to the value's [[toString]] method */
    override def toString() = value.toString
  }

  /** factory object for creating new [[WhenClause]]s */
  object on {
    /** intermediate factory object for creating a new [[WhenClause]] */
    case class OfType[T]() {
      /** constructs a new [[WhenClause]] with the given `action` */
      def apply[Return: TypeTag](action: T => Return)(implicit ev: TypeTag[T]) =
        new Disjunct.WhenClause[T, Return, Return](action)(ev, implicitly[TypeTag[Return]])
    }
    
    /** constructs a new [[WhenClause]] returning the given value */
    def apply[T, Return: TypeTag](typ: T)(value: Return)(implicit ev: TypeTag[T]) =
      new Disjunct.WhenClause[T, Return, Return]({ _ => value })(ev, implicitly[TypeTag[Return]])
    
    /** construct an intermediate [[OfType]] instance for creating a [[WhenClause]] */
    def apply[T] = OfType[T]()
      
  }

  /*object at {
    def apply[K: TypeTag](key: K): AtType[K] = AtValue[K]()

    case class AtValue[K: TypeTag]() {
      def apply[V]: Disjunct.WhenClause[K, Value, Value]
    }
  }*/

  object Conjunct extends Dynamic {
    def apply[K, V](value: V)(implicit typeTag: TypeTag[K], rel: Keying[K, V]): Conjunct[K] =
      new Conjunct[K](Map(TypeIndex(implicitly[TypeTag[K]]) -> value))
  
    def of[K]: Construct[K] = new Construct[K]()
    
    type ValueRelation[K <: AnyRef] = ({ type L[V] = Keying[K, V] })
    def key[K <: AnyRef: TypeTag, V](key: K, value: V)(implicit rel: Keying[key.type, V]): Conjunct[key.type] = Conjunct[key.type, V](value)

    class Construct[K]() {
      def apply[V](value: V)(implicit typeTag: TypeTag[K], rel: Keying[K, V]): Conjunct[K] =
        Conjunct[K, V](value)
    }
  }

  class Conjunct[Type](val values: Map[TypeIndex[_], Any]) extends Dynamic {
   
    def selectDynamic[S <: String: TypeTag, V](key: S)(implicit rel: Keying[key.type, V], ev: Type <:< key.type) = apply[key.type]()

    override def toString: String = values.map(_._2.toString).mkString("{", ", ", "}")
    
    def plus[K: TypeTag, V](value: V)(implicit rel: Keying[K, V],
                                               ev: NotEqual[Type, Type with K]): Conjunct[Type with K] =
      new Conjunct[Type with K](values.updated(TypeIndex(implicitly[TypeTag[K]]), value))
 
    def and[K]: And[K] = new And[K]()
    
    class And[K]() {
      def apply[V](value: V)(implicit typeTag: TypeTag[K],
                                      rel: Keying[K, V],
                                      ev: NotEqual[Type, Type with K]): Conjunct[Type with K] =
        new Conjunct[Type with K](values.updated(TypeIndex(implicitly[TypeTag[K]]), value))
    }
 
    def merge[Ks: TypeTag](that: Conjunct[Ks]): Conjunct[Type with Ks] =
      new Conjunct[Type with Ks](values ++ that.values)
 
    def apply[K]: Apply[K] = new Apply[K]()
    
    class Apply[K]() {
      def apply[V]()(implicit tt: TypeTag[K], ev: Type <:< K, rel: Keying[K, V]): V =
        values(TypeIndex(tt)).asInstanceOf[V]
    }
  
    def subset[Ks](implicit tag: TypeTag[Ks], ev: Type <:< Ks): Conjunct[Ks] =
      new Conjunct[Ks](values.filterKeys { k => tag.tpe <:< k.tag.tpe })
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
  
  def from[T](value: T): Disjunct[Nothing] = macro Macros.coproduct[T]
  
  implicit def sameType[T]: Keying[T, T] = Keying[T, T]()
}

