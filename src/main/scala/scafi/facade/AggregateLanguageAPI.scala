package scafi.facade
import smonads.SMonads.*

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
  def compute[A](a: NValue[A]): Aggregate[A]
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A](a: Aggregate[A])(f: NValue[A] => (Aggregate[A], Aggregate[A])): Aggregate[A]

  type NValue[_]
  given monadNValue: Monad[NValue]

  given toNValue[A]: Conversion[A, NValue[A]]
  def nself[A](a: NValue[A]): NValue[A]
  def nfold[A](init: A)(op: (A, A) => A)(a: NValue[A]): NValue[A]
  extension [A](nv: NValue[A]) def selfValue: A

trait AggregateLanguage extends AggregateLanguageAPI:
  import scafi.core.*
  export Aggregates.Aggregate
  export Aggregates.{sensor, compute, call, exchange}
  export Aggregates.Aggregate.{fromValue, fromNValue}
  import smonads.FreeSMonads.*

  override given monadAggregate: SMonad[Aggregate, NValue] = Aggregates.monadAggregate

  export NValues.NValue
  export NValues.{nself, nfold, selfValue}
  export NValues.given

  override given monadNValue: Monad[NValue] = NValues.nvalueAggregate

object AggregateLanguageModule extends AggregateLanguage