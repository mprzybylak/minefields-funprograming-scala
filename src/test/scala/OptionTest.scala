import org.scalatest.{Matchers, WordSpec}

class OptionTest extends WordSpec with Matchers {

  "An Option" should {

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
}
