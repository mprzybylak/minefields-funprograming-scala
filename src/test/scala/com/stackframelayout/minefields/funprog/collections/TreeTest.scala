package com.stackframelayout.minefields.funprog.collections

import org.scalatest.{Matchers, WordSpec}

class TreeTest extends WordSpec with Matchers {

  import com.stackframelayout.minefields.funprog.collections.Tree._

  "a tree size" should {

    "be equal to 1 for leaf" in {

      // given
      val tree = Leaf(10)

      // when
      val sizeOfTree = treeSize(tree)

      // then
      sizeOfTree shouldEqual 1
    }

    "be equal to 1 for leaf using fold" in {

      // given
      val tree = Leaf(10)

      // when
      val sizeOfTree = treeSize(tree)
      val sizeOfTreeFold = treeSizeFold(tree)

      // then
      sizeOfTreeFold shouldEqual sizeOfTree
    }

    "be equal to 3 for one branch with leafs" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val sizeOfTree = treeSize(tree)

      // then
      sizeOfTree shouldEqual 3
    }

    "be equal to 3 for one branch with leafs using fold" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val sizeOfTree = treeSize(tree)
      val sizeOfTreeFold = treeSizeFold(tree)

      // then
      sizeOfTreeFold shouldEqual sizeOfTree
    }

    "be equal to 7 for full 3 levels tree" in {

      // given
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      // when
      val sizeOfTree = treeSize(tree)

      // then
      sizeOfTree shouldEqual 7
    }

    "be equal to 7 for full 3 levels tree using fold" in {

      // given
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

      // when
      val sizeOfTree = treeSize(tree)
      val sizeOfTreeFold = treeSizeFold(tree)

      // then
      sizeOfTreeFold shouldEqual sizeOfTree
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

    "equals 1 for leaf with value 1 using fold" in {

      // given
      val tree = Leaf(1)

      // when
      val maxiumOfTree = treeMax(tree)
      val maxiumOfTreeFold = treeMaxFold(tree)

      // then
      maxiumOfTreeFold shouldEqual maxiumOfTree
    }

    "equals 2 for branch with leaf equal to 1 and 2" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val maxiumOfTree = treeMax(tree)

      //then
      maxiumOfTree shouldEqual 2
    }

    "equals 2 for branch with leaf equal to 1 and 2 using fold" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val maxiumOfTree = treeMax(tree)
      val maxiumOfTreeFold = treeMaxFold(tree)

      //then
      maxiumOfTreeFold shouldEqual maxiumOfTree
    }


  }

  "a tree depth" should {

    "be equal to 1 for leaf" in {

      // given
      val tree = Leaf(1)

      // when
      val depthOfTree = treeDepth(tree)

      // when
      depthOfTree shouldEqual 1
    }

    "be equal to 1 for leaf using fold" in {

      // given
      val tree = Leaf(1)

      // when
      val depthOfTree = treeDepth(tree)
      val depthOfTreeFold = treeDepthFold(tree)

      // when
      depthOfTree shouldEqual depthOfTreeFold
    }

    "be equal to 2 for branch with leafs" in {

      // given
      val tree = Branch(Leaf(1), Leaf(1))

      // when
      val depthOfTree = treeDepth(tree)

      // then
      depthOfTree shouldEqual 2
    }

    "be equal to 2 for branch with leafs using fold" in {

      // given
      val tree = Branch(Leaf(1), Leaf(1))

      // when
      val depthOfTree = treeDepth(tree)
      val depthOfTreeFold = treeDepthFold(tree)

      // then
      depthOfTree shouldEqual depthOfTreeFold
    }

    "be equal to 3 for branch with leaf on one side and branch with leafs on other" in {

      // given
      val tree = Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))

      // when
      val depthOfTree = treeDepth(tree)

      // when
      depthOfTree shouldEqual 3
    }

    "be equal to 3 for branch with leaf on one side and branch with leafs on other using fold" in {

      // given
      val tree = Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))

      // when
      val depthOfTree = treeDepth(tree)
      val depthOfTreeFold = treeDepthFold(tree)

      // when
      depthOfTree shouldEqual depthOfTreeFold
    }

  }

  "a tree map" should {

    "map int to string in leaf" in {

      // given
      val tree = Leaf(1)

      // when
      val mappedTree = treeMap(tree)(v => v.toString)

      // then
      mappedTree shouldEqual Leaf("1")
    }


    "map int to string in branch with leafs" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val mappedTree = treeMap(tree)(v => v.toString)

      // then
      mappedTree shouldEqual Branch(Leaf("1"), Leaf("2"))
    }

    "multiply each value by 2" in {

      // given
      val tree = Branch(Leaf(1), Leaf(2))

      // when
      val mappedTree = treeMap(tree)(v => v * 2)

      // when
      mappedTree shouldEqual Branch(Leaf(2), Leaf(4))
    }
  }
}
