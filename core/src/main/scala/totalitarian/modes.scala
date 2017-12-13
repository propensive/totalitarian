/*
  Totalitarian, version 1.0.0. Copyright 2010-2017 Jon Pretty, Propensive Ltd.

  The primary distribution site is
  
    http://www.propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  
    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is
  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and limitations under the License.
 */

package totalitarian

import language.{existentials, higherKinds}
import scala.reflect._
import scala.util._
import scala.concurrent._

import annotation.implicitNotFound

trait MethodConstraint

object Mode extends Mode_1 {
  abstract class Import[M[G <: MethodConstraint] <: Mode[G]] {
    def apply[G <: MethodConstraint](): M[G] = modeImplicit[G]
    implicit def modeImplicit[G <: MethodConstraint]: M[G] = mode[G]
    protected def mode[G <: MethodConstraint]: M[G]
  }
}

@implicitNotFound(
    msg = "No implicit mode was available for $"+"{Group} methods. " +
        "Please import a member of totalitarian.mitigation, e.g. mitigation.throwExceptions.")
trait Mode[+Group <: MethodConstraint] { mode =>
  type Wrap[+_, _ <: Exception]
  def wrap[Res, E <: Exception](blk: => Res): Wrap[Res, E]

  def flatWrap[Res, E <: Exception: ClassTag](blk: => Wrap[Res, E]): Wrap[Res, E] =
    wrap(unwrap(blk))

  var callPath = "_"

  def unwrap[Res](value: => Wrap[Res, _ <: Exception]): Res
  def unwrap[Res](value: => Wrap[Res, _ <: Exception], path: String): Res = {
    val oldCallPath = callPath
    callPath += path
    val res = unwrap[Res](value)
    callPath = oldCallPath
    res
  }

  def generic[C <: MethodConstraint]: Mode[C] { type Wrap[+T, E <: Exception] = mode.Wrap[T, E] } =
    this.asInstanceOf[Mode[C] { type Wrap[+T, E <: Exception] = mode.Wrap[T, E] }]

  def compose[Group2 <: MethodConstraint](mode2: Mode[Group2]) = new Mode[Group] {
    type Wrap[+Res, E <: Exception] = mode.Wrap[mode2.Wrap[Res, E], E]

    def wrap[Res, E <: Exception](blk: => Res): Wrap[Res, E] =
      mode.wrap(mode2.wrap(blk))

    def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return =
      mode2.unwrap(mode.unwrap(value))
  }

  def catching[E <: Exception: ClassTag, T](blk: => T) =
    try blk
    catch {
      case e: E => exception(e)
      case e: Exception => throw e
    }

  def safe[T](blk: => T): T = {
    try blk
    catch { case e: Exception => exception(e) }
  }
  def exception[T, E <: Exception: ClassTag](e: E, continue: Boolean = true): T = throw e

  def wrapEither[Res, E <: Exception: ClassTag](blk: => Either[E, Res]): Wrap[Res, E] =
    wrap {
      blk match {
        case Left(e) => throw e
        case Right(r) => r
      }
    }

  def wrapOption[Res](blk: => Option[Res]): Wrap[Res, Exception] = wrap(blk.get)

  def wrapTry[Res, E <: Exception: ClassTag](blk: => Try[Res]): Wrap[Res, E] =
    wrap(blk.get)
}

package mitigation {

  object throwExceptions extends Mode.Import[ThrowExceptionsMode] {
    protected def mode[G <: MethodConstraint] = new ThrowExceptionsMode[G]
  }

  object explicit extends Mode.Import[ExplicitMode] {
    protected def mode[G <: MethodConstraint] = new ExplicitMode[G]
  }

  /*object returnEither extends Mode.Import[ReturnEitherMode] {
    protected def mode[G <: MethodConstraint] = new ReturnEitherMode[G]
  }*/

  /*object returnResult extends Mode.Import[ReturnResultMode] {
    protected def mode[G <: MethodConstraint] = new ReturnResultMode[G]
  }*/

  object returnTry extends Mode.Import[ReturnTryMode] {
    protected def mode[G <: MethodConstraint] = new ReturnTryMode[G]
  }

  object keepCalmAndCarryOn extends Mode.Import[KeepCalmAndCarryOnMode] {
    protected def mode[G <: MethodConstraint] = new KeepCalmAndCarryOnMode[G]()
  }

  object returnOption extends Mode.Import[ReturnOptionMode] {
    protected def mode[G <: MethodConstraint] = new ReturnOptionMode[G]()
  }

  object returnFuture {
    implicit def modeImplicit[G <: MethodConstraint](implicit ec: ExecutionContext) =
      new ReturnFutureMode[G]

    def apply[G <: MethodConstraint](implicit ec: ExecutionContext) = modeImplicit[G]
  }

  class Explicitly[+Res, E <: Exception](blk: => Res) {
    def get: Res = blk
    def opt: Option[Res] = returnOption[Nothing].wrap(blk)
    def getOrElse[Res2 >: Res](t: Res2): Res2 = opt.getOrElse(blk)
    //def either: Either[E, Res] = returnEither[Nothing].wrap(blk)
    def attempt: Try[Res] = returnTry[Nothing].wrap(blk)
    def future(implicit ec: ExecutionContext): Future[Res] = returnFuture[Nothing].wrap(blk)

    override def toString = "<unevaluated result>"
  }

}

private[totalitarian] trait Mode_1 {
  implicit def defaultMode: ThrowExceptionsMode[Nothing] = new ThrowExceptionsMode
}

private[totalitarian] class ThrowExceptionsMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = T2 forSome { type T2 <: T  }
  def wrap[T, E <: Exception](t: => T): T = t
  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value
}

private[totalitarian] class ExplicitMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = mitigation.Explicitly[T, E]

  def wrap[T, E <: Exception](t: => T): mitigation.Explicitly[T, E] =
    new mitigation.Explicitly[T, E](t)

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value.get
}

private[totalitarian] class ReturnTryMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = Try[T]
  def wrap[T, E <: Exception](t: => T): Try[T] = Try(t)

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value.get

  override def toString = "[mitigation.returnTry]"
}

private[totalitarian] class KeepCalmAndCarryOnMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = T2 forSome { type T2 <: T }
  def wrap[T, E <: Exception](t: => T): T =
    try t
    catch { case e: Exception => null.asInstanceOf[T] }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = Option[Return](value).get

  override def toString = "[mitigation.kcaco]"
}

private[totalitarian] class ReturnOptionMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = Option[T]
  def wrap[T, E <: Exception](t: => T): Option[T] =
    try Some(t)
    catch { case e: Exception => None }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value.get

  override def toString = "[mitigation.returnOption]"
}

private[totalitarian] class ReturnFutureMode[+G <: MethodConstraint](implicit ec: ExecutionContext) extends Mode[G] {
  type Wrap[+T, E <: Exception] = Future[T]
  def wrap[T, E <: Exception](t: => T): Future[T] = Future { t }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return =
    Await.result(value, duration.Duration.Inf)

  override def flatWrap[Res, E <: Exception: ClassTag](blk: => Wrap[Res, E]): Wrap[Res, E] = blk

  override def toString = "[mitigation.returnFuture]"
}
