package task1.hierarchy

import cats.kernel.Band
import task1.{Branch, Leaf, Tree}

import scala.util.control.TailCalls._
import java.awt.desktop.AppHiddenListener
import scala.annotation.tailrec

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}

trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  /** Несмотря на название, в этом задании необязательно реализовывать через @tailrec. Но обязательно, чтоб он был
   * стекобезопасным.
   */
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
}

trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object TypeClasses {

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      def dfs(tree: Tree[A]): TailRec[Tree[B]] = tree match {
        case Branch(left, right) => for {
          newLeft <- tailcall(dfs(left))
          newRight <- tailcall(dfs(right))
        } yield Branch(newLeft, newRight)
        case Leaf(v) => done(Leaf(f(v)))
      }

      dfs(fa).result

    }
  }


  implicit val treeApply: Apply[Tree] = new Apply[Tree] {
    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = {
      def dfs(tree: Tree[A => B]): TailRec[Tree[B]] = tree match {
        case Branch(left, right) => for {
          newLeft <- tailcall(dfs(left))
          newRight <- tailcall(dfs(right))
        } yield Branch(newLeft, newRight)
        case Leaf(f) => done(treeFunctor.map(fa)(f))
      }

      dfs(ff).result
    }

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFunctor.map(fa)(f)
  }

  implicit val treeApplicative: Applicative[Tree] = new Applicative[Tree] {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApply.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeApply.map(fa)(f)
  }

  implicit val treeFlatMap: FlatMap[Tree] = new FlatMap[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      def dfs(tree: Tree[A]): TailRec[Tree[B]] = tree match {
        case Branch(left, right) => for {
          newLeft <- tailcall(dfs(left))
          newRight <- tailcall(dfs(right))
        } yield Branch(newLeft, newRight)
        case Leaf(v) => done(f(v))
      }

      dfs(fa).result
    }

    /** Несмотря на название, в этом задании необязательно реализовывать через @tailrec. Но обязательно, чтоб он был
     * стекобезопасным.
     */
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def handler(tree: Tree[Either[A, B]]): TailRec[Tree[B]] = tree match {
        case Leaf(Right(v)) => done(Leaf(v))
        case Leaf(Left(v)) => tailcall(handler(f(v)))
        case Branch(left, right) => for {
          newLeft <- tailcall(handler(left))
          newRight <- tailcall(handler(right))
        } yield Branch(newLeft, newRight)
      }

      handler(f(a)).result
    }


    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeApplicative.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeApplicative.map(fa)(f)
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = treeApplicative.pure(a)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = treeFlatMap.flatMap(fa)(f)

    /** Несмотря на название, в этом задании необязательно реализовывать через @tailrec. Но обязательно, чтоб он был
     * стекобезопасным.
     */
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = treeFlatMap.tailRecM(a)(f)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = treeFlatMap.ap(ff)(fa)

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = treeFlatMap.map(fa)(f)
  }

  //  private val tree: Tree[Int] = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
  //  private val treeFunc: Tree[Int => Int] = Branch(Leaf(_ + 1), Branch(Leaf(_ * 100), Leaf(_ * 2)))
  //  private val mappedTree = treeFunctor.map(tree)(_ + 1)
  //  private val appedTree = treeApply.ap(treeFunc)(tree)
  //  println(appedTree)
  //  println(mappedTree)

}
