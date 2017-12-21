import org.scalatest.{Matchers, WordSpec}

class OptionTest extends WordSpec with Matchers {

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
}
