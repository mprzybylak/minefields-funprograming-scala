package com.stackframelayout.minefields.funprog.errors

import com.stackframelayout.minefields.funprog.errors
import org.scalatest.{Matchers, WordSpec}

class OptionTest extends WordSpec with Matchers {

  import com.stackframelayout.minefields.funprog.errors.Option._

  "map in Option" should {

    "allows to map None to None" in {

      // given
      val option = errors.None

      // when
      val mappedOption = option.map(x => 2)

      // then
      mappedOption shouldEqual errors.None
    }

    "allows to map int to string" in {

      // given
      val option = errors.Some(2)

      // when
      val mappedOption = option.map(x => x.toString)

      // then
      mappedOption shouldEqual errors.Some("2")
    }
  }

  "flat map in Option" should {

    "allows to map None to None" in {

      // given
      val option = errors.None

      // when
      val mappedOption = option.flatMap(x => errors.Some(x))

      // then
      mappedOption shouldEqual errors.None
    }

    "allows to map option to None" in {

      // given
      val option = errors.Some(2)

      // when
      val mappedOption = option.flatMap(x => errors.None)

      // then
      mappedOption shouldEqual errors.None
    }

    "allows to map some value to some other value" in {

      // given
      val option = errors.Some(2)

      // when
      val mappedOption = option.flatMap(x => errors.Some("2"))

      // then
      mappedOption shouldEqual errors.Some("2")
    }
  }

  "get or else in option" should {

    "return defautl value for None" in {

      // given
      val option = errors.None

      // when
      val getOrDefaultValue = option.getOrElse(10)

      // then
      getOrDefaultValue shouldEqual 10
    }

    "return value inside option in case of Some" in {

      // given
      val option = errors.Some(1)

      // wheb
      val getOrDefaultValue = option.getOrElse(100)

      // then
      getOrDefaultValue shouldEqual 1
    }
  }

  "orElse in option" should {

    "return default option in case of None" in {

      // given
      val option = errors.None

      // when
      val orElseValue = option.orElse(errors.Some(2))

      // then
      orElseValue shouldEqual errors.Some(2)
    }

    "return value inside in case of Some" in {

      // given
      val option = errors.Some(3)

      // when
      val orElseValue = option.orElse(errors.Some("A"))

      // then
      orElseValue shouldEqual errors.Some(3)
    }
  }

  "filter in option" should {

    "return None in case of None" in {

      // given
      val option: errors.Option[Int] = errors.None

      // when
      val filteredValue = option.filter(_ > 2)

      // then
      filteredValue shouldEqual errors.None
    }

    "return Some in case of Some matching predicate" in {
      // given
      val option = errors.Some(2)

      // when
      val filteredValue = option.filter(_ == 2)

      // when
      filteredValue shouldEqual errors.Some(2)
    }

    "return None in case of Some not-matching predicate" in {

      // given
      val option = errors.Some(2)

      // when
      val filteredValue = option.filter(_ != 2)

      // then
      filteredValue shouldEqual errors.None

    }
  }

  "variance" should {

    "be not calculated for empty" in {

      // given
      val seq = Seq()

      // when
      val v = variance(seq)

      // then
      v shouldEqual errors.None
    }

    "be calciulated for one element sequence" in {

      // given
      val seq = Seq(2.0)

      // when
      val v = variance(seq)

      // then
      v shouldEqual errors.Some(0.0)

    }

    "be calculated for two elements sequence" in {

      // given
      val seq = Seq(2.0, 4.0)

      // when
      val v = variance(seq)

      // then
      v shouldEqual errors.Some(1.0)
    }
  }

  "lift" should {

    "create abs function that returns None for None" in {

      // given
      val absLift = lift(math.abs)

      // when
      val absOption = absLift(errors.None)

      // then
      absOption shouldEqual errors.None
    }

    "create abs function that returns abs value for Some" in {

      // given
      val absLift = lift(math.abs)

      // when
      val absOption = absLift(errors.Some(-3))

      // then
      absOption shouldEqual errors.Some(3)
    }
  }

  "map2 function" should {

    "return none for two Nones" in {

      // given
      val first: errors.Option[Int] = errors.None
      val second: errors.Option[Int] = errors.None

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual errors.None
    }

    "return None if first argument is None" in {

      // given
      val first: errors.Option[Int] = errors.None
      val second: errors.Option[Int] = errors.Some(3)

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual errors.None
    }

    "return None if second argument is None" in {

      // given
      val first: errors.Option[Int] = errors.Some(3)
      val second: errors.Option[Int] = errors.None

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual errors.None
    }

    "map both options with function in case of two Some" in {

      val first: errors.Option[Int] = errors.Some(3)
      val second: errors.Option[Int] = errors.Some(6)

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual errors.Some(9)
    }
  }

  "sequence function" should {

    "combine one Some to sequence" in {

      // given
      val sequenceOfOptions = scala.collection.immutable.List(errors.Some(3))

      // when
      val s = sequence(sequenceOfOptions)

      // then
      s shouldEqual errors.Some(scala.collection.immutable.List(3))
    }

    "combine two Some to sequence" in {

      // given
      val sequenceOfOptions = scala.collection.immutable.List(errors.Some(3), errors.Some(4))

      // when
      val s = sequence(sequenceOfOptions)

      // then
      s shouldEqual errors.Some(scala.collection.immutable.List(3, 4))
    }

    "create none if one of the inputs of two elements sequence is none" in {

      // given
      val sequenceOfOptions = scala.collection.immutable.List(errors.Some(3), errors.None)

      // when
      val s = sequence(sequenceOfOptions)

      // then
      s shouldEqual errors.None
    }

    "create none if one of the inputs of multiple elements sequence is none" in {

      // given
      val sequenceOfOptions = scala.collection.immutable.List(errors.Some(1), errors.Some(2), errors.Some(3), errors.None, errors.Some(4), errors.Some(5))

      // when
      val s = sequence(sequenceOfOptions)

      // then
      s shouldEqual errors.None
    }

    "create empty list if input list is empty" in {

      // given
      val sequenceOfOptions = scala.collection.immutable.List()

      // when
      val s = sequence(sequenceOfOptions)

      // then
      s shouldEqual errors.Some(scala.collection.immutable.List())
    }

  }
}
