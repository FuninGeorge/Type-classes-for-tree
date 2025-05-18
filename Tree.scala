package task1

import task1.hierarchy.{Applicative, Apply, FlatMap, Functor, Monad}

import scala.util.control.TailCalls.{TailRec, done, tailcall}

sealed trait Tree[+T]

case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[+T](value: T) extends Tree[T]
