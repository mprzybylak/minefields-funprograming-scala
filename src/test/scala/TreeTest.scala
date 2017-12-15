import org.scalatest.{Matchers, WordSpec}

import Tree._

class TreeTest extends WordSpec with Matchers {

  "a tree size" should {

    "be equal to 1 for leaf" in {

      // given
      val tree = Leaf(10)

      // when
      val sizeOfTree = treeSize(tree)

      // then
      sizeOfTree shouldEqual 1
    }

    "be equal to 3 for one branch with leafs" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val sizeOfTree = treeSize(tree)

      // then
      sizeOfTree shouldEqual 3
    }

    "be equal to 7 for full 3 levels tree" in {

      // given
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      // when
      val sizeOfTree = treeSize(tree)

      // then
      sizeOfTree shouldEqual 7
    }
  }

  "a tree maximum" should {

    "equals 1 for leaf with value 1" in {

      // given
      val tree = Leaf(1)

      // when
      val maxiumOfTree = treeMax(tree)

      // then
      maxiumOfTree shouldEqual 1
    }

    "equals 2 for branch with leaf equal to 1 and 2" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val maxiumOfTree = treeMax(tree)

      //then
      maxiumOfTree shouldEqual 2
    }



  }


}
