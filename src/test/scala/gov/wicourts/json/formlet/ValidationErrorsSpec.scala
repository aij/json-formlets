package gov.wicourts.json.formlet

import org.specs2.mutable.Specification

import scalaz.NonEmptyList

import argonaut._
import argonaut.Argonaut.jNull

import scalaz.syntax.monoid._

class ValidationErrorsSpec extends Specification {
  "Fields errors" >> {
    "can render themselves to JSON" >> {
      val errors = ValidationErrors.field(NonEmptyList("a", "b"))

      errors.toJson.nospaces must_== """["a","b"]"""
    }

    "can be added" >> {
      val e1 = ValidationErrors.field(NonEmptyList("a"))
      val e2 = ValidationErrors.field(NonEmptyList("b"))

      (e1 |+| e2).toJson.nospaces must_== """["a","b"]"""
    }
  }

  "Object errors" >> {
    val errors = ValidationErrors.obj(List(
      "field1" ->  ValidationErrors.field(NonEmptyList("a")),
      "field2" ->  ValidationErrors.field(NonEmptyList("b")),
      "obj1" -> ValidationErrors.obj(List(
        "field3" -> ValidationErrors.field(NonEmptyList("c"))
      ))
    ))

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """{"field1":["a"],"field2":["b"],"obj1":{"field3":["c"]}}"""
    }

    "can be added" >> {
      val other = ValidationErrors.obj(List(
        "field1" ->  ValidationErrors.field(NonEmptyList("d")),
        "field4" ->  ValidationErrors.field(NonEmptyList("b")),
        "obj1" -> ValidationErrors.obj(List(
          "field3" -> ValidationErrors.field(NonEmptyList("x"))
        ))
      ))

      (errors |+| other).toJson.nospaces must_== """{"field1":["a","d"],"field2":["b"],"obj1":{"field3":["c","x"]},"field4":["b"]}"""
    }
  }

  "Array errors" >> {
    val errors = ValidationErrors.array(List(
      1 -> ValidationErrors.field(NonEmptyList("a")),
      99 -> ValidationErrors.field(NonEmptyList("b"))
    ))

    "can render themselves to JSON" >> {
      errors.toJson.nospaces must_== """[[1,["a"]],[99,["b"]]]"""
    }

    "can be added" >> {
      val other = ValidationErrors.array(List(
        1 -> ValidationErrors.field(NonEmptyList("a")),
        10 -> ValidationErrors.field(NonEmptyList("b"))
      ))
      (errors |+| other).toJson.nospaces must_== """[[1,["a","a"]],[99,["b"]],[10,["b"]]]"""
    }
  }
}
