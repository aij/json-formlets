package gov.wicourts.json.formlet

import scala.language.implicitConversions
import scala.language.higherKinds

import scalaz.Functor

class ObjectFormletOps[M[_], A](self: ObjectFormlet[M, A])(implicit M: Functor[M]) {
  def required[B](name: String)(implicit ev: A <:< Option[B]): ObjectFormlet[M, B] =
    Forms.requiredObj(name, self.map(a => a: Option[B]))

  def fromRoot: ObjectFormlet[M, A] = Forms.fromRoot(self)
}

class FieldFormletOps[M[_], A](self: FieldFormlet[M, A])(implicit M: Functor[M]) {
  def obj: ObjectFormlet[M, A] = Forms.obj[M, A](self)
  def label(s: String): FieldFormlet[M, A] = Forms.label(self, s)
  def errorName(s: String): FieldFormlet[M, A] = Forms.errorName(self, s)

  def required[B](implicit ev: A <:< Option[B]): FieldFormlet[M, B] =
    Forms.required(self.map(a => a: Option[B]))

  def fromRoot: FieldFormlet[M, A] = Forms.fromRoot(self)
}

class IdFieldFormletOps[A](self: IdFieldFormlet[A]) {
  def obj: IdObjectFormlet[A] = Forms.obj(self)
  def label(s: String): IdFieldFormlet[A] = Forms.label(self, s)
  def errorName(s: String): IdFieldFormlet[A] = Forms.errorName(self, s)

  def required[B](implicit ev: A <:< Option[B]): IdFieldFormlet[B] =
    Forms.required(self.map(a => a: Option[B]))

  def fromRoot: IdFieldFormlet[A] = Forms.fromRoot(self)
}

trait ToFieldFormletOps0 {
  implicit def FieldFormletToFieldFormletOps[M[_] : Functor, A](
    v: FieldFormlet[M, A]
  ): FieldFormletOps[M, A] =
    new FieldFormletOps[M, A](v)
}

trait ToFieldFormletOps extends ToFieldFormletOps0 {
  implicit def IdFieldFormletToIdFieldFormletOps[A](v: IdFieldFormlet[A]): IdFieldFormletOps[A] =
    new IdFieldFormletOps(v)
}

trait ToObjectFormletOps {
  implicit def ObjectFormletToObjectFormletOps[M[_] : Functor, A](
    v: ObjectFormlet[M, A]
  ): ObjectFormletOps[M, A] =
    new ObjectFormletOps[M, A](v)
}
