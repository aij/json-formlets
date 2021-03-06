package gov.wicourts.json.formlet

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.Apply
import scalaz.syntax.monoid._
import scalaz.syntax.monad._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

import argonaut.Json.jNumber

import org.scalacheck.{Gen, Arbitrary}

class JsonBuilderSpec extends Specification with ScalaCheck {
  "JsonArrayBuilder" >> {
    "Type class laws" >> {
      implicit val arbitraryJsonArrayBuilder: Arbitrary[JsonArrayBuilder] = Arbitrary(
        Gen.listOf(Gen.choose(1, 100)).map(l => new JsonArrayBuilder(l.map(jNumber(_))))
      )

      "Monoid" >> {
        monoid.laws[JsonArrayBuilder]
      }

      "Equal" >> {
        equal.laws[JsonArrayBuilder]
      }
    }
  }

  "JsonObjectBuilder" >> {
    "Type class laws" >> {
      implicit val arbitraryJsonObjectBuilder: Arbitrary[JsonObjectBuilder] = Arbitrary(
        Gen.listOf(Apply[Gen].tuple2(Gen.alphaStr, Gen.choose(1, 100))).map(l =>
          new JsonObjectBuilder(l.map { case (s, n) => (s, jNumber(n)) })
        )
      )

      "Monoid" >> {
        monoid.laws[JsonObjectBuilder]
      }

      "Equal" >> {
        equal.laws[JsonObjectBuilder]
      }
    }
  }
}
