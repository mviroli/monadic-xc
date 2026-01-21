package scafi.facadeMonadXC

import fplib.SMonads.*
import scafi.core

/**
 *  The facade to the API includes:
 *  - a trait with the contract
 *  - an implementing trait exporting from "core" implementation
 *  - an object to import from
 */

trait AggregateLanguageAPI:
  type XC[_]
  given monadAggregate: SMonad[XC, NValue]

  given fromValue[A]: Conversion[A, XC[A]]
  given fromNValue[A]: Conversion[NValue[A], XC[A]]
  def sensor[A](a: => A): XC[A]
  def compute[A](a: A): XC[A]
  def call[A](f: XC[() => XC[A]]): XC[A]
  def exchange[A](a: XC[A])(f: XC[A] => (XC[A], XC[A])): XC[A]
  def fold[A](init: XC[A])(op: (A, A) => A)(a: XC[A]): XC[A]
  def self[A](a: XC[A]): XC[A]

  type NValue[_]
  given monadNValue: Monad[NValue]


trait AggregateLanguage extends AggregateLanguageAPI:
  import scafi.core.*
  override type XC[A] = Aggregate[A]
  export AggregateConstructs.Aggregate
  import fplib.FreeSMonads.*

  override given monadAggregate: SMonad[XC, NValue] = AggregateConstructs.monadAggregate
  override given fromValue[A]: Conversion[A, XC[A]] = AggregateConstructs.Aggregate.fromValue
  override given fromNValue[A]: Conversion[NValue[A], XC[A]] = AggregateConstructs.Aggregate.fromNValue

  export NValueConstructs.NValue
  export NValueConstructs.{selfValue}
  export NValueConstructs.given

  import NValueConstructs.{nself, nfold}

  def sensor[A](a: => A): XC[A] = Aggregate.sensor(a)

  def compute[A](a: A): XC[A] = Aggregate.compute(a)

  def call[A](f: XC[() => XC[A]]): XC[A] = Aggregate.call(f)

  def exchange[A](a: XC[A])(f: XC[A] => (XC[A], XC[A])): XC[A] =
    Aggregate.exchange(a)(nv => f(nv))

  def fold[A](init: XC[A])(op: (A, A) => A)(a: XC[A]): XC[A] =
    for
      i <- init
      v <- a
    yield nfold(i.selfValue)(op)(v)

  def self[A](a: XC[A]): XC[A] =
    for 
      v <- a
    yield nself(v)

  override given monadNValue: Monad[NValue] = NValueConstructs.nvalueAggregate

object AggregateLanguageModule extends AggregateLanguage