package gov.wicourts.json

import argonaut.Cursor

import scalaz.Id.Id

import scala.language.higherKinds

import scalaz.NonEmptyList

package object formlet {
  type IdFieldFormlet[A] = Formlet[Id, Option[Cursor], NonEmptyList[String], A, FieldView]
  type IdObjectFormlet[A] = Formlet[Id, Option[Cursor], ValidationErrors, A, JsonObjectBuilder]

  type FieldFormlet[M[_], A] = Formlet[M, Option[Cursor], NonEmptyList[String], A, FieldView]
  type ObjectFormlet[M[_], A] = Formlet[M, Option[Cursor], ValidationErrors, A, JsonObjectBuilder]

  type JsonFormlet[M[_], E, A, V] = Formlet[M, Option[Cursor], E, A, V]

  object syntax extends ToFieldFormletOps with ToObjectFormletOps
}
