sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def treeSize[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => treeSize(left) + treeSize(right) + 1
  }

  def treeMax(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => treeMax(left).max(treeMax(right))
  }

  def treeDepth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + treeDepth(l).max(treeDepth(r))
  }

  def treeMap[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(treeMap(l)(f), treeMap(r)(f))
  }
}