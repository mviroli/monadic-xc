package scafi

import org.scalatest.Assertion
import AggregatesNFWithFree.{*, given}
import Tree.*

object AggregateTestUtilities:
  type DomainChange = PartialFunction[Int, Domain]
  given DomainChange = Map.empty
  type TreeChange[A] = PartialFunction[Int, Tree[A]]
  given tc[A]: TreeChange[A] = Map.empty

  def withDomainChange(domainChange: DomainChange)(body: DomainChange ?=> Assertion): Assertion = body(using domainChange)
  def withTreeChange[A](treeChange: TreeChange[A])(body: TreeChange[A] ?=> Assertion): Assertion = body(using treeChange)

  extension [A](a: Aggregate[A])
    def repeat(initial: Tree[A] = TEmpty[A](), dom: Domain = Set(selfDevice))
              (using domainChange: DomainChange)(using treeChange: TreeChange[A]): LazyList[Tree[A]] =
      LazyList.iterate((-1, initial)):
        (step, tree) => (step + 1, a.foldMap(compiler).apply(using domainChange.applyOrElse(step + 1, _ => dom))(tree))
      .map(_._2).drop(1)

    def evalOne(initial: Tree[A] = TEmpty(), dom: Domain = Set(selfDevice)): Tree[A] =
      repeat(initial, dom)(0)

