package com.stackframelayout.minefields.funprog.stream

import com.stackframelayout.minefields.funprog.Stream.emptyStream
import com.stackframelayout.minefields.funprog.{Cons, Empty, Stream}
import org.scalatest.{Matchers, WordSpec}

class StreamTest extends WordSpec with Matchers {

  import com.stackframelayout.minefields.funprog.Stream._

  "stream" should {

    "allows to be empty" in {

      // when
      val stream = emptyStream

      // then
      stream shouldBe Empty
    }

    "allow to have one element" in {

      // when
      val stream = consStream(1, emptyStream)

      // then
      stream shouldNot be(emptyStream)
    }

    "return empty option for head for empty stream" in {

      // given
      val stream = emptyStream

      // when
      val head = headOption(stream)

      // then
      head shouldBe None
    }

    "return Some option for head for non-empty stream" in {

      // given
      val stream = Stream(1)

      // when
      val head = headOption(stream)

      // then
      head shouldBe Some(1)
    }
  }

  "conversion to list" should {

    "convert empty stream to list" in {

      // given
      val stream = Stream()

      // when
      val list = stream.toList

      // then
      list shouldBe Nil
    }

    "convert one element stream to one element list" in {

      // given
      val stream = Stream(1)

      // when
      val list = stream.toList

      // then
      list shouldBe List(1)
    }

    "convert two elements sztream to two elements list" in {

      // given
      val stream = Stream(1, 2)

      // when
      val list = stream.toList

      // then
      list shouldBe List(1, 2)
    }
  }

  "take method" should {

    "return empty list for empty list" in {

      // given
      val stream = Stream()

      // when
      val takeStream = stream.take(1)

      // then
      takeStream shouldBe Empty
    }

    "return full stream for stream when take size is equal size of stream" in {

      // given
      val stream = Stream(1, 2, 3)

      // when
      val takeStream = stream.take(3)

      // then
      takeStream.toList shouldEqual List(1,2,3)
    }

    "return full stream for stream when take size is bigger than size of stream" in {

      // given
      val stream = Stream(1, 2, 3)

      // when
      val takeStream = stream.take(100)

      // then
      takeStream.toList shouldEqual List(1, 2, 3)
    }

    "return subset of stream for stream when take size is smaller than size of stream" in {

      // given
      val stream = Stream(1, 2, 3, 4, 5)

      // when
      val takeStream = stream.take(2)

      // then
      takeStream.toList shouldEqual List(1, 2)
    }
  }

}
