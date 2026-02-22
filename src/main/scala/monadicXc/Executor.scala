package monadicXc

trait Executor:
  this: MonadicFramework =>

  type DomainReset = List[Int]
  given dc: DomainReset = List()

  def withDomainReset(domainReset: DomainReset)(body: DomainReset ?=> Unit): Unit = body(using domainReset)
  def evalOnce[A](a: Aggregate[A])(map: Map[Device, Message] = Map()): A = a.run(selfDevice)(map)._2
  def evalOnce[A](a: Aggregate[A]): A = a.run(selfDevice)(Map())._2

  def repeat[A](a: Aggregate[A])(using DomainReset): LazyList[A] =
    trace(a).map((c,n,m) => n)

  def trace[A](a: Aggregate[A])(using DomainReset): LazyList[(Context, A, Message)] =
    var ct = 0
    LazyList.iterate(a.run(selfDevice)(Map())): cn =>
      ct = ct + 1
      a.run(selfDevice)(if summon[DomainReset].contains(ct) then Map() else Map(selfDevice -> cn._3))

  case class Displacement[P](positions: Map[Device, P], topology: Map[Device, Set[Device]]):
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

  case class Platform(topology: Map[Device, Set[Device]] = Map(), ssns: Map[String, Map[Device, Any]] = Map()):
    def withSensor[B](name: String, values: Map[Device, B], default: B = null): Platform =
      val toAdd = if default == null then Map() else Map.from(topology.keys.map(_ -> NValue(default)))
      var newValues = (toAdd ++ values.map((k, v) => (k, v))).asInstanceOf[Map[Device, Any]]
      this.copy(ssns = ssns + (name -> newValues))

    def withNeighbourhood(mapping: (Device, Set[Device])): Platform = this.copy(topology = topology + mapping)

    def withTopology(topology: Map[Device, Set[Device]]): Platform = this.copy(topology = topology)

    def asDistributedSystem[A](aggr: DistributedSystem[A] ?=> Aggregate[A]): DistributedSystem[A] =
      val ds = new DistributedSystem[A](this)
      ds.aggregate = aggr(using ds)
      ds


  class DistributedSystem[A](val platform: Platform):
    var aggregate: Aggregate[A] = _
    var envs: Map[Device, Context] = platform.topology.view.mapValues(_ => Map()).toMap//topology.keySet.map(d => d -> Map(d -> emptyMessage())).toMap.asInstanceOf[Map[Device, Context]]
    private var _snss: Map[String, Map[Device, Any]] = Map()
    private var _currentDevice = selfDevice

    def withAggregate(aggr: DistributedSystem[A] ?=> Aggregate[A]): this.type = try this finally this.aggregate = aggr(using this)

    def withSensor[B](name: String, values: Map[Device, B]): this.type = try this finally _snss = _snss + (name -> values)

    def fire(d: Device): A =
      _currentDevice = d
      val (ctx, a, msg) = aggregate.run(_currentDevice)(envs(d))
      envs = platform.topology(d).foldLeft(envs)((e, dd) => e + (dd -> (envs(dd) + (d -> msg))))
      a

    def fires(ds: Device*): Seq[A] = ds.map(fire(_))
  object DistributedSystem:
    def platformSensor[A, B](name: String): DistributedSystem[B] ?=> Aggregate[A] =
      sensor:
        () => summon[DistributedSystem[B]].platform.ssns(name)(summon[DistributedSystem[B]]._currentDevice).asInstanceOf[A]

  export DistributedSystem.platformSensor