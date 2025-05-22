package scafi

import org.scalatest.Assertion
import Aggregates.{*, given}
import Tree.*

object AggregateTestUtilities:
  type DomainChange = PartialFunction[Int, Domain]
  given dc: DomainChange = Map.empty
  type TreeChange[A] = PartialFunction[Int, Tree[A]]
  given tc[A]: TreeChange[A] = Map.empty

  def withDomainChange(domainChange: DomainChange)(body: DomainChange ?=> Assertion): Assertion = body(using domainChange)
  def withTreeChange[A](treeChange: TreeChange[A])(body: TreeChange[A] ?=> Assertion): Assertion = body(using treeChange)

  extension [A](a: Aggregate[A])
    def repeat(using device: Device = selfDevice)
              (initial: Environment[A] = localEnv(TEmpty[A]()), dom: Domain = Set(device))
              (using domainChange: DomainChange = dc)
              (using treeChange: TreeChange[A] = tc[A]): LazyList[Tree[A]] =
      LazyList.iterate((-1, initial)): (step, context) =>
        val preEnv = restrictEnv(context)(domainChange.applyOrElse(step + 1, _ => context.keySet))
        val env = if preEnv.isEmpty then localEnv(TEmpty[A]()) else preEnv
        (step + 1, localEnv(a.round(env)))
      .map(_._2).map(_(device)).drop(1)

    def evalOne(using device: Device)(initial: Environment[A] = localEnv(TEmpty[A]()), dom: Domain = Set(device)): Tree[A] =
      a.round(initial)
      //repeat(initial, dom)(0)


  class DistributedSystem[A](aggregate: Aggregate[A], topology: Map[Device, Domain]):
    var envs: Map[Device, Environment[A]] = topology.keySet.map(d => (d, Map(d -> TEmpty[A]()))).toMap
    def fire(d: Device): Tree[A] =
      val tree = aggregate.evalOne(using d)(envs(d), topology(d))
      envs = topology(d).foldLeft(envs)((e, dd) => e + (dd -> (envs(dd) + (d -> tree))))
      tree