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

  class DistributedSystem[A](var aggregate: Aggregate[A], topology: Map[Device, Set[Device]]):
    var envs: Map[Device, Context] = topology.view.mapValues(_ => Map()).toMap//topology.keySet.map(d => d -> Map(d -> emptyMessage())).toMap.asInstanceOf[Map[Device, Context]]
    private var _snss: Map[String, Map[Device, Any]] = Map()
    private var _currentDevice = selfDevice

    def this(topology: Map[Device, Set[Device]]) = this(null, topology)

    def withAggregate(aggr: DistributedSystem[A] ?=> Aggregate[A]): this.type = try this finally this.aggregate = aggr(using this)

    def withSensor[B](name: String, values: Map[Device, B]): this.type = try this finally _snss = _snss + (name -> values)

    def fire(d: Device): A =
      _currentDevice = d
      val (ctx, a, msg) = aggregate.run(_currentDevice)(envs(d))
      envs = topology(d).foldLeft(envs)((e, dd) => e + (dd -> (envs(dd) + (d -> msg))))
      a

    /*
    def fires(ds: Device*): Seq[Export[A]] = ds.map(fire(_))

  object DistributedSystem:
    def platformSensor[A, B](name: String): DistributedSystem[B] ?=> () => A = () => summon[DistributedSystem[B]]._snss(name)(summon[DistributedSystem[B]]._currentDevice).asInstanceOf[A]
    */