package com.stackframelayout.minefields.funprog.collections

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def treeSize[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => treeSize(left) + treeSize(right) + 1
  }

  def treeSizeFold[A](tree: Tree[A]): Int = {
    treeFold(tree, 0)((a, b) => if(b == 0 ) 1 else b + 2)
  }

  def treeMax(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => treeMax(left).max(treeMax(right))
  }

  def treeMaxFold(tree: Tree[Int]): Int = treeFold(tree, Int.MinValue)((a,b) => a.max(b))

  def treeDepth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + treeDepth(l).max(treeDepth(r))
  }

  def treeDepthFold[A](tree: Tree[A]): Int = treeFold(tree, 0)((a,b) => b + 1)

  def treeMap[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(treeMap(l)(f), treeMap(r)(f))
  }

  def treeFold[A,B](tree: Tree[A], z: B)(f: (A,B) => B): B = tree match {
    case Leaf(v) => f(v,z)
    case Branch(l, r) => treeFold(l, treeFold(r, z)(f))(f)
  }
}