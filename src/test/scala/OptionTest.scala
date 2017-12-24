import org.scalatest.{Matchers, WordSpec}

class OptionTest extends WordSpec with Matchers {

  import Option._

  "map in Option" should {

    "allows to map None to None" in {

      // given
      val option = None

      // when
      val mappedOption = option.map(x => 2)

      // then
      mappedOption shouldEqual None
    }

    "allows to map int to string" in {

      // given
      val option = Some(2)

      // when
      val mappedOption = option.map(x => x.toString)

      // then
      mappedOption shouldEqual Some("2")
    }
  }

  "flat map in Option" should {

    "allows to map None to None" in {

      // given
      val option = None

      // when
      val mappedOption = option.flatMap(x => Some(x))

      // then
      mappedOption shouldEqual None
    }

    "allows to map option to None" in {

      // given
      val option = Some(2)

      // when
      val mappedOption = option.flatMap(x => None)

      // then
      mappedOption shouldEqual None
    }

    "allows to map some value to some other value" in {

      // given
      val option = Some(2)

      // when
      val mappedOption = option.flatMap(x => Some("2"))

      // then
      mappedOption shouldEqual Some("2")
    }
  }

  "get or else in option" should {

    "return defautl value for None" in {

      // given
      val option = None

      // when
      val getOrDefaultValue = option.getOrElse(10)

      // then
      getOrDefaultValue shouldEqual 10
    }

    "return value inside option in case of Some" in {

      // given
      val option = Some(1)

      // wheb
      val getOrDefaultValue = option.getOrElse(100)

      // then
      getOrDefaultValue shouldEqual 1
    }
  }

  "orElse in option" should {

    "return default option in case of None" in {

      // given
      val option = None

      // when
      val orElseValue = option.orElse(Some(2))

      // then
      orElseValue shouldEqual Some(2)
    }

    "return value inside in case of Some" in {

      // given
      val option = Some(3)

      // when
      val orElseValue = option.orElse(Some("A"))

      // then
      orElseValue shouldEqual Some(3)
    }
  }

  "filter in option" should {

    "return None in case of None" in {

      // given
      val option: Option[Int] = None

      // when
      val filteredValue = option.filter(_ > 2)

      // then
      filteredValue shouldEqual None
    }

    "return Some in case of Some matching predicate" in {
      // given
      val option = Some(2)

      // when
      val filteredValue = option.filter(_ == 2)

      // when
      filteredValue shouldEqual Some(2)
    }

    "return None in case of Some not-matching predicate" in {

      // given
      val option = Some(2)

      // when
      val filteredValue = option.filter(_ != 2)

      // then
      filteredValue shouldEqual None

    }
  }

  "variance" should {

    "be not calculated for empty" in {

      // given
      val seq = Seq()

      // when
      val v = variance(seq)

      // then
      v shouldEqual None
    }

    "be calciulated for one element sequence" in {

      // given
      val seq = Seq(2.0)

      // when
      val v = variance(seq)

      // then
      v shouldEqual Some(0.0)

    }

    "be calculated for two elements sequence" in {

      // given
      val seq = Seq(2.0, 4.0)

      // when
      val v = variance(seq)

      // then
      v shouldEqual Some(1.0)
    }
  }

  "lift" should {

    "create abs function that returns None for None" in {

      // given
      val absLift = lift(math.abs)

      // when
      val absOption = absLift(None)

      // then
      absOption shouldEqual None
    }

    "create abs function that returns abs value for Some" in {

      // given
      val absLift = lift(math.abs)

      // when
      val absOption = absLift(Some(-3))

      // then
      absOption shouldEqual Some(3)
    }
  }

  "map2 function" should {

    "return none for two Nones" in {

      // given
      val first: Option[Int] = None
      val second: Option[Int] = None

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual None
    }

    "return None if first argument is None" in {

      // given
      val first: Option[Int] = None
      val second: Option[Int] = Some(3)

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual None
    }

    "return None if second argument is None" in {

      // given
      val first: Option[Int] = Some(3)
      val second: Option[Int] = None

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual None
    }

    "map both options with function in case of two Some" in {

      val first: Option[Int] = Some(3)
      val second: Option[Int] = Some(6)

      // when
      val result = map2(first, second)((a,b) => a + b)

      // then
      result shouldEqual Some(9)
    }
  }
}
