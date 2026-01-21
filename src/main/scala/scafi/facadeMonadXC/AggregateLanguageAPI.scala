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
  type Aggregate[_]
  given monadAggregate: SMonad[Aggregate, NValue]

  given fromValue[A]: Conversion[A, Aggregate[A]]
  given fromNValue[A]: Conversion[NValue[A], Aggregate[A]]
  def sensor[A](a: => A): Aggregate[A]
  def compute[A](a: A): Aggregate[A]
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A](a: Aggregate[A])(f: Aggregate[A] => (Aggregate[A], Aggregate[A])): Aggregate[A]
  def fold[A](init: Aggregate[A])(op: (A, A) => A)(a: Aggregate[A]): Aggregate[A]
  def self[A](a: Aggregate[A]): Aggregate[A]

  type NValue[_]
  given monadNValue: Monad[NValue]

  given toNValue[A]: Conversion[A, NValue[A]]
  //def nself[A](a: NValue[A]): NValue[A]
  //def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A]
  //extension [A](nv: Aggregate[A]) def selfValue: A

trait AggregateLanguage extends AggregateLanguageAPI:
  import scafi.core.*
  export AggregateConstructs.Aggregate.{fromValue, fromNValue}
  export AggregateConstructs.Aggregate
  import fplib.FreeSMonads.*

  override given monadAggregate: SMonad[Aggregate, NValue] = AggregateConstructs.monadAggregate

  export NValueConstructs.NValue
  export NValueConstructs.{selfValue}
  export NValueConstructs.given

  import NValueConstructs.{nself, nfold}

  def sensor[A](a: => A): Aggregate[A] = Aggregate.sensor(a)

  def compute[A](a: A): Aggregate[A] = Aggregate.compute(a)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = Aggregate.call(f)

  def exchange[A](a: Aggregate[A])(f: Aggregate[A] => (Aggregate[A], Aggregate[A])): Aggregate[A] =
    Aggregate.exchange(a)(nv => f(nv))

  def fold[A](init: Aggregate[A])(op: (A, A) => A)(a: Aggregate[A]): Aggregate[A] =
    for
      i <- init
      v <- a
    yield nfold(i.selfValue)(op)(v)

  def self[A](a: Aggregate[A]): Aggregate[A] =
    for 
      v <- a
    yield nself(v)

  override given monadNValue: Monad[NValue] = NValueConstructs.nvalueAggregate

object AggregateLanguageModule extends AggregateLanguage