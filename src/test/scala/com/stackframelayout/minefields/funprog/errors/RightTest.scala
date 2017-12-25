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

  "flat map" should {

    "map Left to Left" in {

      // given
      val either:Either[String, Int] = Left("error")

      // when
      val newEither = either.flatMap(v => Right(v + 1))

      // then
      newEither shouldEqual Left("error")
    }

    "map Right value using given function" in {

      // given
      val either:Either[String, Int] = Right(10)

      // when
      val newEither = either.flatMap(v => Right(v + 1))

      // then
      newEither shouldEqual Right(11)
    }
  }

  "orElse" should {

    "return new Either in case of Left" in {

      // given
      val either: Either[String, Int] = Left("error")

      // when
      val newEither = either.orElse(Right("a"))

      // then
      newEither shouldEqual Right("a")
    }

    "return new Left in case of Left" in {

      // given
      val either: Either[String, Int] = Left("error")

      // when
      val newEither = either.orElse(Left("super error"))

      // then
      newEither shouldEqual Left("super error")
    }

    "return old Either in case of Right" in {

      // given
      val either: Either[String, Int] = Right(10)

      // when
      val newEither = either.orElse(Right(100))

      // then
      newEither shouldEqual Right(10)
    }
  }
}
