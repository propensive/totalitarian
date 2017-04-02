package totalitarian

import scala.reflect.runtime.universe.TypeTag
import scala.language.existentials
import scala.language.higherKinds
import scala.annotation.implicitNotFound

object Disjunct {
  
  final case class OfType[Type]() {
    def apply[V](value: V)(implicit evidence: Type <:< V, typeTag: TypeTag[V]): Disjunct[Type] =
      new Disjunct(value, typeTag)
  }
  
  def of[Type]: OfType[Type] = OfType()
    
  def apply[Type](value: Type)(implicit typeTag: TypeTag[Type]): Disjunct[Type] =
    new Disjunct(value, typeTag)

  trait ResultType[T1, T2] {
    type Wrap[T1, T2]
    def wrap(value: T1, typeTag: TypeTag[_]): Wrap[T1, T2]
  }

  object ResultType extends ResultType_1 {
    
    implicit def equalTypes[T1, T2](implicit ev: T1 =:= T2): ResultType[T1, T2] {
        type Wrap[T1, T2] = T2 } = new ResultType[T1, T2] {
      
      type Wrap[T1, T2] = T2
    
      def wrap(value: T1, typeTag: TypeTag[_]): T2 = value
    }
  }

  trait ResultType_1 {
    implicit def unequalTypes[T1, T2]: ResultType[T1, T2] { type Wrap[T1, T2] = Disjunct[T2] } =
      new ResultType[T1, T2] {
        type Wrap[T1, T2] = Disjunct[T2]
        def wrap(value: T1, typeTag: TypeTag[_]): Disjunct[T2] =
          new Disjunct(value, typeTag)
      }
  }

  case class CaseClause[-T: TypeTag, +R, -Return: TypeTag](val action: T => R) {
    val fromTypeTag: TypeTag[_] = implicitly[TypeTag[T]]
    val toTypeTag: TypeTag[_] = implicitly[TypeTag[Return]]
  }

  object AllCases {
    implicit def allCases[T1, T2](implicit ev: T1 <:< T2): AllCases[T1, T2] = AllCases()
  }

  @implicitNotFound("not all cases have been specified")
  case class AllCases[T1, +T2]()

}

class Disjunct[-Type](val value: T forSome { type T >: Type }, val typeTag: TypeTag[_]) {
  
  def apply[T, R, R2](cases: Disjunct.CaseClause[T, R, R2]*)(implicit ev: Disjunct.AllCases[T, Type],
      resultType: Disjunct.ResultType[R, R2]): resultType.Wrap[R, R2] = {
    
    val thisCase = cases.find(_.fromTypeTag.tpe <:< typeTag.tpe).get
    resultType.wrap(thisCase.action(value.asInstanceOf[T]), thisCase.toTypeTag)
  }

  override def toString() = value.toString
}

object on {
  case class OfType[T]() {
    def apply[Return: TypeTag](action: T => Return)(implicit ev: TypeTag[T]) =
      new Disjunct.CaseClause[T, Return, Return](action)(ev, implicitly[TypeTag[Return]])
  }
  
  def apply[T] = OfType[T]()
    
}
