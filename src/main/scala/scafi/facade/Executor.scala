package scafi.facade

import scafi.facade.AggregateEngineModule.{*, given}

/**
 * Utilities to run rounds, mainly for simulation/testing
 */

object Executor:
  type Domain = Set[Device]
  type DomainChange = PartialFunction[Int, Domain]
  given dc: DomainChange = Map.empty
  type TreeChange[A] = PartialFunction[Int, Export[A]]
  given tc[A]: TreeChange[A] = Map.empty

  def withDomainChange(domainChange: DomainChange)(body: DomainChange ?=> Unit): Unit = body(using domainChange)
  def withTreeChange[A](treeChange: TreeChange[A])(body: TreeChange[A] ?=> Unit): Unit = body(using treeChange)
  def restrictEnv[A](c: Environment[A])(domain: Domain): Environment[A] = c.filter((d, _) => domain.contains(d))

  extension [A](a: Aggregate[A])
    def repeat(using device: Device = selfDevice)
              (initial: Environment[A] = Map(device -> initialExport[A]), dom: Domain = Set(device))
              (using domainChange: DomainChange = dc)
              (using treeChange: TreeChange[A] = tc[A]): LazyList[Export[A]] =
      LazyList.iterate((-1, initial)): (step, context) =>
        val preEnv = restrictEnv(context)(domainChange.applyOrElse(step + 1, _ => context.keySet))
        val env = if preEnv.isEmpty then Map(device -> initialExport[A]) else preEnv
        (step + 1, Map(device -> round(a)(device)(env)))
      .map(_._2).map(_(device)).drop(1)

    def evalOne(using device: Device)(initial: Environment[A] = Map(device -> initialExport[A]), dom: Domain = Set(device)): Export[A] =
      round(a)(device)(initial)

  class DistributedSystem[A](aggregate: Aggregate[A], topology: Map[Device, Domain]):
    var envs: Map[Device, Environment[A]] = topology.keySet.map(d => (d, Map(d -> initialExport[A]))).toMap
    def fire(d: Device): Export[A] =
      val tree = aggregate.evalOne(using d)(envs(d), topology(d))
      envs = topology(d).foldLeft(envs)((e, dd) => e + (dd -> (envs(dd) + (d -> tree))))
      tree

    def fires(ds: Device*): Seq[Export[A]] = ds.map(fire(_))

@main def debugger =
  import Executor.*
  import scafi.lib.AggregateLib.counter
  val nv: NValue[Int] = 1
  val nv2: NValue[Int] = for n <- nv yield n + 1
  val ag: Aggregate[Int] = nv2
  println:
    ag
  println:
    ag.repeat().take(1).map(_.top.asValue).toList


