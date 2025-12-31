package scafifc_experimental

import fplib.SMonads.*
import scafi.core.NValueConstructs.NValue
import scafi.facade.{AggregateEngine, AggregateEngineAPI}
import scafi.utils.MapWithDefault

trait FCLanguage:
  type Field[_]
  given monadField: Monad[Field]

  type NbrMap[_]
  given monadNbrMap: Monad[NbrMap]
  type NbrField[_]
  given smonadNbrField: SMonad[NbrField, NbrMap]

  given fieldFromValue[A]: Conversion[A, Field[A]]
  given nbrFromValue[A]: Conversion[A, NbrField[A]]

  def localSensor[A](a: => A): Field[A]
  def nbrSensor[A](a: NbrMap[A]): NbrField[A]

  def rep[A](a: =>A)(f: A => Field[A]): Field[A]
  def nbr[A](a: Field[A]): NbrField[A]
  def fold[A](init: A)(op: (A, A) => A)(nbr: NbrField[A]): Field[A]
  def branch[A](cond: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A]


trait FCLanguageImpl extends FCLanguage:
  import scafi.facade.AggregateLanguageModule as ALM

  override type Field[A] = ALM.Aggregate[A]
  given monadField: Monad[Field] with
    def pure[A](a: A): Field[A] = ALM.fromValue(a)
    extension [A](ma: Field[A]) def flatMap[B](f: A => Field[B]): Field[B] =
      ALM.monadAggregate.flatMap(ma)(nv => f(ALM.selfValue(nv)))

  override type NbrMap[A] = ALM.NValue[A]

  given monadNbrMap: Monad[NValue] = ALM.monadNValue
  case class NbrField[A](a: ALM.Aggregate[A])
  import scafi.core.NValueSemantics.asNbrMap
  given smonadNbrField: SMonad[NbrField, NbrMap] with
    def pure[A](a: NbrMap[A]): NbrField[A] = NbrField(ALM.monadAggregate.pure(a))
    extension [A](ma: NbrField[A])
      def flatMap[B](f: NbrMap[A] => NbrField[B]): NbrField[B] =
        NbrField(ALM.monadAggregate.flatMap(ma.a)(nm => f(nm).a))

  given fieldFromValue[A]: Conversion[A, Field[A]] = ALM.fromValue
  given nbrFromValue[A]: Conversion[A, NbrField[A]] = a => NbrField(ALM.fromValue(a))

  def localSensor[A](a: => A): Field[A] = ALM.sensor(a)
  def nbrSensor[A](a: NbrMap[A]): NbrField[A] = NbrField(ALM.compute(a))

  def rep[A](a: => A)(f: A => Field[A]): Field[A] = ALM.exchange(ALM.compute(a))(nv => (f(ALM.selfValue(nv)), f(ALM.selfValue(nv))))
  def nbr[A](a: Field[A]): NbrField[A] = NbrField(ALM.exchange(a)(nv => (nv, a)))
  def fold[A](init: A)(op: (A, A) => A)(nbr: NbrField[A]): Field[A] =
    smonadNbrField.map(nbr)(nv => ALM.nfold(init)(op)(nv)).a // smonadNbrField.flatMap(init)(nvi => smonadNbrField.map(nbr)(nv => ALM.nfold(nvi)(op)(nv)).a)
  def branch[A](cond: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A] =
    scafi.lib.AggregateLib.branch(cond)(th)(el)

def fcLib(fc: FCLanguage) =
  import fc.{*, given}
  def nbrRange: NbrField[Double] = ???

  def mux[A](cond: Field[Boolean])(th: Field[A])(el: Field[A]): Field[A] =
    for
      c <- cond
      t <- th
      e <- el
    yield if c then t else e

  def gradient(src: Field[Boolean]): Field[Double] =
    rep(Double.PositiveInfinity): d =>
      mux[Double](src)(0.0):
        fold(Double.PositiveInfinity)(_ min _):
          for
            dd <- nbr(d)
            md <- nbrRange
          yield for
            d <- dd
            m <- md
          yield d + m

trait FCEngine extends AggregateEngineAPI with FCLanguageImpl:
  import scafi.core.Environments
  type Aggregate[A] = Field[A]
  type Environment[A] = Environments.Environment[A]
  type Export[A] = Environments.Tree[A]

  override def initialExport[A]: Export[A] = Environments.Tree.TEmpty[A]()

  export Environments.Devices.{Device, newDevice, selfDevice}

  import scafi.core.AggregateSemantics.round

  override def round[A](a: Field[A])(d: Device)(e: Environment[A]): Export[A] = a.round(using e)(using d).asInstanceOf[Export[A]]

object FCEngineModule extends FCEngine



def directLists =
  trait Ctx[T[_]]
  def element[T[_], A](using Ctx[T])(a: T[A]): A = ???
  def give[T[_], A](using Ctx[T])(a: =>A): A = ???

  def compose[T[_], A](body: Ctx[T] ?=> Unit) = ???
  given Ctx[List] = ???

  compose[List, Int]:
    var e1 = element(List(10,20,30))
    var e2 = element(List(40, 50))
    give:
      e1 + e2

