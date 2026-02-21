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
