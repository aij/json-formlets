package gov.wicourts.json

import argonaut.Cursor

import scalaz.Id.Id

import scala.language.higherKinds

import scalaz.NonEmptyList

package object formlet {

  // A formlet taking JSON as input.
  type JsonFormlet[M[_], E, A, V] = Formlet[M, Option[Cursor], E, A, V]

  // A JSON formlet that works on a single, leaf field.
  type FieldFormlet[M[_], A] = JsonFormlet[M, NonEmptyList[String], A, FieldView]
  // A JSON formlet that outputs a JSON object.
  type ObjectFormlet[M[_], A] = JsonFormlet[M, ValidationErrors, A, JsonObjectBuilder]

  // Unused garbage?
  type IdFieldFormlet[A] = FieldFormlet[Id, A]
  // Unused garbage?
  type IdObjectFormlet[A] = ObjectFormlet[Id, A]


  object syntax extends ToFieldFormletOps with ToObjectFormletOps
}
