package scafi.facadeMonadXC

import scafi.facade.AggregateEngineModule.{*, given}
import scafi.utils.MapWithDefault

import scala.reflect.ClassTag
import scala.util.Random

/**
 * Utilities to run rounds, mainly for simulation/testing
 */

object Executor:
  type Domain = Set[Device]
  type DomainChange = PartialFunction[Int, Domain]
  given dc: DomainChange = Map.empty
  type TreeChange[A] = PartialFunction[Int, Export[A]]
  given tc[A]: TreeChange[A] = Map.empty
  val random = new Random

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

  case class Displacement[P](positions: Map[Device, P], topology: Map[Device, Domain]):
    def apply(p: P): Device = positions.find((k, v) => v == p).get._1
    def apply(d: Device): P = positions(d)

  object Displacement:
    def grid(n: Int, m: Int): Displacement[(Int, Int)] =
      val positions = (for x <- 0 until n; y <- 0 until m yield newDevice() -> (x, y)).toMap
      val topology = (for
        d1 -> (x1, y1) <- positions.toSet
        d2 -> (x2, y2) <- positions.toSet
        if (x1, y1) == (x2, y2) || Math.abs(x2 - x1) + Math.abs(y2 - y1) == 1
      yield d1 -> d2).groupMap(_._1)(_._2)
      Displacement(positions, topology)

  given valueToMap[A]: Conversion[A, MapWithDefault[Device, A]] = MapWithDefault(_)
  case class Platform(topology: Map[Device, Domain] = Map(), ssns: Map[String, Map[Device, NValue[Any]]] = Map()):
    def withSensor[B](name: String, values: Map[Device, MapWithDefault[Device, B]], default: B = null): Platform  =
      val toAdd = if default == null then Map() else Map.from(topology.keys.map(_ -> NValue(default)))
      var newValues = (toAdd ++ values.map((k,v)=>(k,NValue(v)))).asInstanceOf[Map[Device, NValue[Any]]]
      this.copy(ssns = ssns + (name -> newValues))

    def withNeighbourhood(mapping: (Device, Domain)): Platform = this.copy(topology = topology + mapping)

    def withTopology(topology: Map[Device, Domain]): Platform = this.copy(topology = topology)

    def asDistributedSystem[A](aggr: DistributedSystem[A] ?=> Aggregate[A]): DistributedSystem[A] =
      val ds = new DistributedSystem[A](this)
      ds.aggregate = aggr(using ds)
      ds

  class DistributedSystem[A](val platform: Platform):
    var aggregate: Aggregate[A] = _
    var envs: Map[Device, Environment[A]] = platform.topology.keySet.map(d => (d, Map(d -> initialExport[A]))).toMap
    private var _currentDevice = selfDevice

    def fire(d: Device): Export[A] =
      _currentDevice = d
      val tree = aggregate.evalOne(using d)(envs(d), platform.topology(d))
      envs = platform.topology(d).foldLeft(envs)((e, dd) => e + (dd -> (envs(dd) + (d -> tree))))
      tree

    def fires(ds: Device*): Seq[Export[A]] = ds.map(fire(_))

    def randomFire(): (Device, Export[A]) =
      val randomDevice = platform.topology.keySet.toSeq(random.nextInt(platform.topology.size))
      randomDevice -> fire(randomDevice)

  object DistributedSystem:
    def platformSensor[A, B](name: String): DistributedSystem[B] ?=> Aggregate[A] =
      sensor:
        summon[DistributedSystem[B]].platform.ssns(name)(summon[DistributedSystem[B]]._currentDevice).asInstanceOf[NValue[A]]



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


