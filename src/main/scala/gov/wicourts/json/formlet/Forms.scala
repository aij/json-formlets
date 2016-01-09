package gov.wicourts.json.formlet

import argonaut._
import argonaut.Argonaut._

import scalaz._
import scalaz.OptionT.optionT
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.tuple._
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

import scalaz.Id.Id

import scala.language.higherKinds

object Forms {
  private def primitive[M[_]: Applicative, A](
    descr: String,
    toJson: A => Json,
    matches: Json => Boolean,
    fromJson: Json => NonEmptyList[String] \/ A,
    name: String,
    value: Option[A]
  ): FieldFormlet[M, Option[A]] =
    Formlet { c =>
      val result = optionT[NonEmptyList[String] \/ ?](
        c
          .flatMap(_.downField(name))
          .map(_.focus)
          .filterNot(_.isNull)
          .right[NonEmptyList[String]]
      ).flatMapF(j =>
        j.right[NonEmptyList[String]].ensure(
          NonEmptyList(s"Field $name must be a(n) $descr")
        )(
          matches
        ) >>= (fromJson)
      ).run.validation

      val view = FieldView(name, (result.toOption.join orElse value).map(toJson), None, name)

      (result, view).point[M]
    }

  private def namedContext[M[_], E, A, V](
    name: String,
    inner: JsonFormlet[M, E, A, V]
  ): JsonFormlet[M, E, A, V] =
    Formlet(c => inner.run(c.flatMap(_.downField(name))))

  def nestedM[M[_] : Functor, A, V <: JsonBuilder](
    name: String,
    inner: JsonFormlet[M, ValidationErrors, A, V]
  ): ObjectFormlet[M, A] =
    namedContext(name, inner).mapResult((r, v) => (
      r.leftMap(x => ValidationErrors.obj(List((name, x)))),
      JsonObjectBuilder.row(name, v.toJson)
    ))

  def listM[M[_], A](
    template: ObjectFormlet[M, A],
    defaultValue: List[ObjectFormlet[M, A]]
  )(
    implicit M: Applicative[M]
  ): JsonFormlet[M, ValidationErrors, List[A], JsonArrayBuilder] =
    Formlet { c =>
      val l: List[(Option[Cursor], ObjectFormlet[M, A])] =
        if (c.isEmpty)
          defaultValue.map((c, _))
        else {
          type X = Vector[(Option[Cursor], ObjectFormlet[M, A])]
          c
            .flatMap(_.downArray)
            .map(
              _.traverseBreak(
                Kleisli[State[X, ?], Cursor, Option[Cursor]](c =>
                  State(l => (l :+ (c.some, template), c.left)))
              ).apply(Vector()).toList
            )
            .getOrElse(Nil)
        }

      val X = M
        .compose[Tuple2[JsonArrayBuilder, ?]]
        .compose[Validation[ValidationErrors, ?]]
      M.map(X.traverse(l.zipWithIndex) { case ((i, x), idx) =>
        M.map(x.mapResult((r, v) => (
          r.leftMap(o => ValidationErrors.array(List((idx, o)))),
          JsonArrayBuilder.item(v.toJson)
        )).run(i))(_.swap)
      })(_.swap)
    }

  private def check[A](name: String, descr: String, a: Option[A]): NonEmptyList[String] \/ A =
    a.toRightDisjunction(NonEmptyList(s"Expected a $descr when processing field $name"))

  private def fromArray[A](
    name: String,
    descr: String,
    fromItem: Json => Option[A]
  ): Json => NonEmptyList[String] \/ List[A] = j =>
    check(name, s"array of $descr", j.array) >>= (_.traverseU(i => check(name, descr, fromItem(i))))

  def listOfStringM[M[_] : Applicative](
    name: String,
    value: Option[List[String]]
  ): FieldFormlet[M, Option[List[String]]] =
    primitive(
      "array of string",
      l => Json.array(l.map(jString(_)): _*),
      _.isArray,
      fromArray(name, "string", _.string),
      name,
      value
    )

  def listOfNumberM[M[_] : Applicative](
    name: String,
    value: Option[List[JsonNumber]]
  ): FieldFormlet[M, Option[List[JsonNumber]]] =
    primitive(
      "array of number",
      l => Json.array(l.map(jNumber(_)): _*),
      _.isArray,
      fromArray(name, "number", _.number),
      name,
      value
    )

  def listOfBooleanM[M[_] : Applicative](
    name: String,
    value: Option[List[Boolean]]
  ): FieldFormlet[M, Option[List[Boolean]]] =
    primitive(
      "array of boolean",
      l => Json.array(l.map(jBool(_)): _*),
      _.isArray,
      fromArray(name, "boolean", _.bool),
      name,
      value
    )

  def stringM[M[_] : Applicative](
    name: String,
    value: Option[String]
  ): FieldFormlet[M, Option[String]] =
    primitive(
      "string",
      jString(_),
      _.isString,
      ((_: Json).string.map(_.trim)) andThen (a => check(name, "string", a)),
      name,
      value
    )

  def numberM[M[_] : Applicative](
    name: String,
    value: Option[JsonNumber]
  ): FieldFormlet[M, Option[JsonNumber]] =
    primitive(
      "number",
      jNumber(_),
      _.isNumber,
      ((_: Json).number) andThen (a => check(name, "number", a)),
      name,
      value
    )

  def booleanM[M[_] : Applicative](
    name: String,
    value: Option[Boolean]
  ): FieldFormlet[M, Option[Boolean]] =
    primitive(
      "boolean",
      jBool(_),
      _.isBool,
      ((_: Json).bool) andThen (a => check(name, "boolean", a)),

      name,
      value
    )

  def label[M[_] : Functor, A](
    field: FieldFormlet[M, A],
    label: String
  ): FieldFormlet[M, A] =
    field.mapView(FieldView.label.set(_, label.some))

  def errorName[M[_] : Functor, A](
    field: FieldFormlet[M, A],
    errorName: String
  ): FieldFormlet[M, A] =
    field.mapView(FieldView.errorName.set(_, errorName))

  def required[M[_] : Functor, A](
    field: FieldFormlet[M, Option[A]]
  ): FieldFormlet[M, A] =
    field.mapValidation(_.toSuccess(NonEmptyList("This field is required")))

  def requiredObj[M[_] : Functor, A](
    name: String,
    obj: ObjectFormlet[M, Option[A]]
  ): ObjectFormlet[M, A] =
    obj.mapValidation(_.toSuccess(ValidationErrors.string(name, "This field is required")))

  def obj[M[_] : Functor, A](
    field: FieldFormlet[M, A]
  ): ObjectFormlet[M, A] =
    field.mapResult((a, v) => (
      a.leftMap(l => ValidationErrors.list(v.errorName, l)),
      v.obj
    ))

  object Id {
    def listOfString(
      name: String,
      value: Option[List[String]]
    ): IdFieldFormlet[Option[List[String]]] =
      listOfStringM(name, value)

    def listOfNumber(
      name: String,
      value: Option[List[JsonNumber]]
    ): IdFieldFormlet[Option[List[JsonNumber]]] =
      listOfNumberM(name, value)

    def listOfBoolean(
      name: String,
      value: Option[List[Boolean]]
    ): IdFieldFormlet[Option[List[Boolean]]] =
      listOfBooleanM(name, value)

    def string(
      name: String,
      value: Option[String]
    ): IdFieldFormlet[Option[String]] =
      stringM(name, value)

    def number(
      name: String,
      value: Option[JsonNumber]
    ): IdFieldFormlet[Option[JsonNumber]] =
      numberM(name, value)

    def boolean(
      name: String,
      value: Option[Boolean]
    ): IdFieldFormlet[Option[Boolean]] =
      booleanM(name, value)

    def list[E, A](
      template: IdObjectFormlet[A],
      defaultValue: List[IdObjectFormlet[A]]
    ): JsonFormlet[Id, ValidationErrors, List[A], JsonArrayBuilder] =
      listM[Id, A](template, defaultValue)

    def nested[A, V <: JsonBuilder](
      name: String,
      inner: JsonFormlet[Id, ValidationErrors, A, V]
    ): IdObjectFormlet[A] =
      nestedM[Id, A, V](name, inner)
  }
}
