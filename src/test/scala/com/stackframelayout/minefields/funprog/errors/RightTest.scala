package com.stackframelayout.minefields.funprog.errors

import org.scalatest.{Matchers, WordSpec}

class RightTest extends WordSpec with Matchers {

  "map method" should {

    "map Left to Left" in {

      // given
      val either:Either[String, Int] = Left("error")

      // when
      val newEither = either.map(v => v + 1)

      // then
      newEither shouldEqual Left("error")
    }

    "map Right value using given function" in {

      // given
      val either:Either[String, Int] = Right(10)

      // when
      val newEither = either.map(v => v + 1)

      // then
      newEither shouldEqual Right(11)
    }
  }
}
