package gov.wicourts.json.formlet

import scalaz._
import scalaz.Liskov._
import scalaz.Id.Id

import scalaz.NonEmptyList.nel

import scalaz.std.function._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.monoid._
import scalaz.syntax.validation._

import scala.language.higherKinds

import Predef.identity

/**
  A helper class to help define different formlet implementations.

  A wrapper around functions that should be interpreted as formlets.

  This is based on "The Essense of Form Abstraction", Ezra Cooper et al.

  Rather than providing a concrete implementation, we based this off
  of the factored formlets implementation, while leaving the
  implementation of Namer, Environment, and XmlWriter abstract. You
  can think of this kind of as a functor (in the ML sense), which will
  give you a formlet implementation when given suitable
  implementations of Namer, Environment, and XmlWriter.

  @param M If you actually want the result to be a proper formlet, this should be a an Applicative Functor (aka Idiom). Use Id to match the paper, but you can use other type constructors like Function[Foo, ?] or Kleisli if you would like the run function to take additional arguments.
  @param I The type of the environment? Use Option[Cursor] for [JsonFormlet]s
  @param E The type used to represent errors.
  @param A The type of value stored in the formlet. (Ie, the α in `type α t` from the paper.)
  @param V The view type. As in, the output that will be sent to the user. (aka xml in the paper)
  @param run The run function from the formlet paper. Rather than supplying one yourself, you can build a formlet through the helper function in ???.
  */
case class Formlet[M[_], I, E, A, V](run: I => M[(Validation[E, A], V)]) {
  def eval(i: I)(implicit M: Functor[M]): M[Validation[E, A]] =
    M.map(run(i))(_._1)

  def view(i: I)(implicit M: Functor[M]): M[V] =
    M.map(run(i))(_._2)

  def bimap[B, C](f: E => B, g: A => C)(implicit M: Functor[M]): Formlet[M, I, B, C, V] =
    mapResult((result, view) => (result bimap (f, g), view))

  def leftMap[B](f: E => B)(implicit M: Functor[M]): Formlet[M, I, B, A, V] =
    bimap(f, identity)

  def map[B](f: A => B)(implicit M: Functor[M]): Formlet[M, I, E, B, V] =
    mapResult((result, view) => (result map f, view))

  def mapK[G[_], EE, AA, VV](
    f: M[(Validation[E, A], V)] => G[(Validation[EE, AA], VV)]
  ): Formlet[G, I, EE, AA, VV] =
    Formlet(f compose run)

  def mapK_[G[_], B](
    f: M[(Validation[E, A], V)] => G[(Validation[E, B], V)]
  ): Formlet[G, I, E, B, V] = mapK(f)

  def ap[B](
    f: => Formlet[M, I, E, A => B, V]
  )(
    implicit E: Semigroup[E], V: Monoid[V], M: Applicative[M]
  ): Formlet[M, I, E, B, V] =
    Formlet(c =>
      M.apply2(this.run(c), f.run(c)) { case ((a, v1), (ff, v2)) =>
        (a <*> ff, v1 |+| v2)
      }
    )

  def mapView[U](f: V => U)(implicit M: Functor[M]): Formlet[M, I, E, A, U] =
    mapResult((result, view) => (result, f(view)))

  def mapResult[EE, AA, W](
    f: (Validation[E, A], V) => (Validation[EE, AA], W)
  )(
    implicit M: Functor[M]
  ): Formlet[M, I, EE, AA, W] =
    Formlet(c => M.map(run(c))(f.tupled))

  def mapValidation[B](
    f: A => Validation[E, B]
  )(
    implicit M: Functor[M]
  ): Formlet[M, I, E, B, V] = {
    mapResult((a, v) => ((a.disjunction.flatMap(f(_).disjunction)).validation, v))
  }

  def validate[B](
    h: A => Validation[E, B],
    t: (A => Validation[E, B])*
  )(
    implicit E: Semigroup[E], M: Applicative[M]
  ): Formlet[M, I, E, B, V] = {
    val X = Applicative[A => ?].compose[Validation[E, ?]]
    val f = X.sequence(nel(h, t.toList))
    mapValidation(f).map(_.head)
  }

  def mapResultM[EE, AA, W](
    f: (Validation[E, A], V) => M[(Validation[EE, AA], W)]
  )(
    implicit M: Bind[M]
  ): Formlet[M, I, EE, AA, W] =
    Formlet(c => M.bind(run(c))(f.tupled))

  def mapValidationM[B](
    f: A => M[Validation[E, B]]
  )(
    implicit M: Monad[M]
  ): Formlet[M, I, E, B, V] = {
    mapResultM((a, v) =>
      M.map(
        EitherT(M.point(a.disjunction))
          .flatMapF(x => M.map(f(x))(_.disjunction))
          .run
      )(
        r => (r.validation, v)
      )
    )
  }

  def validateM[B](
    h: A => M[Validation[E, B]],
    t: (A => M[Validation[E, B]])*
  )(
    implicit E: Semigroup[E], M: Monad[M]
  ): Formlet[M, I, E, B, V] = {
    val X = Applicative[A => ?].compose[M].compose[Validation[E, ?]]
    val f = X.sequence(nel(h, t.toList))
    mapValidationM(f).map(_.head)
  }

  def value(implicit M: Functor[M]): I => M[Option[A]] = i =>
    M.map(this.eval(i))(_.toOption)

  def valueOpt[B](implicit M: Functor[M], ev: A <~< Option[B]): I => M[Option[B]] = i =>
    M.map(this.eval(i))(v => Monad[Option].join(v.toOption.map(ev(_))))

  def validateVM[B, C](
    other: I => M[B]
  )(
    h: (B, A) => M[Validation[E, C]],
    t: ((B, A) => M[Validation[E, C]])*
  )(
    implicit E: Semigroup[E], M: Monad[M]
  ): Formlet[M, I, E, C, V] = Formlet { c =>
    M.bind(other(c)) { b =>
      val h1 = h(b, _: A)
      val t1 = t.map(f => f(b, _: A))
      validateM(h1, t1: _*).run(c)
    }
  }

  def validateV[B, C](
    other: I => B
  )(
    h: (B, A) => Validation[E, C],
    t: ((B, A) => Validation[E, C])*
  )(
    implicit E: Semigroup[E], M: Applicative[M]
  ): Formlet[M, I, E, C, V] = Formlet { c =>
    val b = other(c)
    val h1 = h(b, _: A)
    val t1 = t.map(f => f(b, _: A))
    validate(h1, t1: _*).run(c)
  }

  def lift[L[_] : Applicative]: Formlet[Lambda[a => L[M[a]]], I, E, A, V] =
    Formlet[Lambda[a => L[M[a]]], I, E, A, V](c => Applicative[L].point(run(c)))

  def liftId[L[_] : Applicative](
    implicit ev: this.type <~< Formlet[Id, I, E, A, V]
  ): Formlet[L, I, E, A, V] =
    Formlet[L, I, E, A, V](c => Applicative[L].point(ev(this).run(c)))

  def local[X](f: X => I): Formlet[M, X, E, A, V] = Formlet(run compose f)

  def contramap[X](f: X => I): Formlet[M, X, E, A, V] = local(f)
}

object Formlet {
  implicit def formletContravariant[M[_], E, A, V]: Contravariant[Formlet[M, ?, E, A, V]] =
    new Contravariant[Formlet[M, ?, E, A, V]] {
      def contramap[X, XX](fa: Formlet[M, X, E, A, V])(f: XX => X): Formlet[M, XX, E, A, V] =
        fa contramap f
    }

  implicit def formletBifunctor[M[_] : Functor, I, V ]: Bifunctor[Formlet[M, I, ?, ?, V]] =
    new Bifunctor[Formlet[M, I, ?, ?, V]] {
      def bimap[A, B, C, D](fab: Formlet[M, I, A, B, V])(f: A => C, g: B => D): Formlet[M, I, C, D, V] =
        fab.bimap(f, g)
    }

  implicit def formletApplicative[
    M[_] : Applicative,
    I,
    E : Semigroup,
    V : Monoid
  ]: Applicative[Formlet[M, I, E, ?, V]] =
    new Applicative[Formlet[M, I, E, ?, V]] {
      override def map[A, B](a: Formlet[M, I, E, A, V])(f: A => B): Formlet[M, I, E, B, V] =
        a map f

      override def point[A](a: => A): Formlet[M, I, E, A, V] =
        Formlet[M, I, E, A, V](_ => Applicative[M].point((a.success, Monoid[V].zero)))

      override def ap[A, B](
        fa: => Formlet[M, I, E, A, V]
      )(
        f: => Formlet[M, I, E, A => B, V]
      ): Formlet[M, I, E, B, V] =
        fa ap f
    }
}
