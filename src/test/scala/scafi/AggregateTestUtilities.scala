package scafi

import org.scalatest.Assertion
import Aggregates.{*, given}
import Tree.*

object AggregateTestUtilities:
  type DomainChange = PartialFunction[Int, Domain]
  given DomainChange = Map.empty
  type TreeChange[A] = PartialFunction[Int, Tree[A]]
  given tc[A]: TreeChange[A] = Map.empty

  def withDomainChange(domainChange: DomainChange)(body: DomainChange ?=> Assertion): Assertion = body(using domainChange)
  def withTreeChange[A](treeChange: TreeChange[A])(body: TreeChange[A] ?=> Assertion): Assertion = body(using treeChange)

  extension [A](a: Aggregate[A])
    def repeat(using device: Device)
              (initial: Context[A] = localContext(TEmpty[A]()), dom: Domain = Set(device))
              (using domainChange: DomainChange)(using treeChange: TreeChange[A]): LazyList[Tree[A]] =
      LazyList.iterate((-1, initial)): (step, context) =>
        val preCtx = restrict(context)(domainChange.applyOrElse(step + 1, _ => context.keySet))
        val ctx = if preCtx.isEmpty then localContext(TEmpty[A]()) else preCtx
        (step + 1, localContext(a.round(ctx)))
      .map(_._2).map(_(device)).drop(1)

    def evalOne(using device: Device)(initial: Context[A] = localContext(TEmpty[A]()), dom: Domain = Set(device)): Tree[A] =
      repeat(initial, dom)(0)

