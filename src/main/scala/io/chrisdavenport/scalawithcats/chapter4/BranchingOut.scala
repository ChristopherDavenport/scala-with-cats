package io.chrisdavenport.scalawithcats.chapter4

import cats._
import scala.annotation.tailrec
import cats.implicits._

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  // NOT MY FINAL INSTANCE
  // val treeMonad : Monad[Tree] = new Monad[Tree]{
  //   override def pure[A](a: A) = leaf(a)
  //   override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
  //     case Leaf(a) => f(a)
  //     case Branch(l,r) => Branch(flatMap(l)(f), flatMap(r)(f))
  //   }
  //   // Not Stack Safe
  //   override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
  //     case Leaf(Left(a)) => tailRecM(a)(f)
  //     case Leaf(Right(b)) => Leaf(b)
  //     case Branch(left, right) => {
  //       val leftBranch = flatMap(left){
  //         case Left(a) => tailRecM(a)(f)
  //         case Right(b) => Leaf(b)
  //       }
  //       val rightBranch = flatMap(right){
  //         case Left(a) => tailRecM(a)(f)
  //         case Right(b) => Leaf(b)
  //       }
  //       Branch(leftBranch, rightBranch)
  //     }
  //   }
  // }

  // Final Instance
  implicit val finalInstancesForTree : SemigroupK[Tree] 
  with Traverse[Tree]
  with CoflatMap[Tree]
  with Monad[Tree] = new SemigroupK[Tree] 
  with Traverse[Tree]
  with CoflatMap[Tree]
  with Monad[Tree] {
    override def pure[A](a: A) = leaf(a)
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a) => f(a)
      case Branch(l,r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Tree[B]]): List[Tree[B]] = open match {
        // Head of List is a Branch
        // Important Job is decomposing the branches and add those to
        // the list of open trees
        case Branch(l, r) :: next => 
          l match {
            // Left is also a branch, add this explicitly to the list of open trees 
            case Branch(_, _) => 
              loop(l :: r :: next, closed)
            // Left is untransformed Leaf, transform and then add to stack
            case Leaf(Left(a)) => 
              loop(f(a) :: r :: next, closed)
            // Left Value Is Transformed B, Move onto Closed List
            case Leaf(Right(b)) =>
              loop(r :: next, pure(b) :: closed)
          }
        // Head of List is Untransformed Leaf
        case Leaf(Left(a)) :: next  =>
          loop(f(a):: next, closed)
        // Head of List is transformed
        // Must Rebuild Tree Out of Trees
        case Leaf(Right(b)) :: next => 
          closed match {
            // Upon Two Values in Stack, Combine them Into A Single Tree
            case head :: tail =>
              loop(next, Branch(head, pure(b)) :: tail)
            // Empty Closed So Just Add The New Tree
            case Nil => 
              loop(next, pure(b):: Nil)
          }
        // Empty List Return List of Closed
        case Nil => closed
      }
      // Initiate With The Single Value -> 
      // Empty is Closed
      // Head As We should End With a Single Value in the Closed List As We Combine on more than one into a Branch
      loop(f(a) :: Nil , List.empty[Tree[B]]).head
    }

    def coflatMap[A, B](fa: Tree[A])(f: Tree[A] => B): Tree[B] = leaf(f(fa))

    def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = fa match {
      case Leaf(a) => f(b, a)
      case Branch(l, r) => foldLeft(r, foldLeft(l, b)(f))(f)
    }
    def foldRight[A, B](fa: Tree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      fa match {
        case Leaf(a) => f(a, lb)
        case Branch(l, r) => foldRight(r,foldRight(l, lb)(f))(f)
      }
    }
    
    def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = fa match {
      case Leaf(a) => f(a).map(b => Tree.leaf(b))
      case Branch(l, r) => Apply[G].map2(traverse(l)(f), traverse(r)(f))(Branch(_, _))
    }

    def combineK[A](x: Tree[A],y: Tree[A]): Tree[A] = branch(x, y)

  }

  def fill[A](n: Int)(a: A): Tree[A] = List.fill(n - 1)(a.pure[Tree]).foldRight(a.pure[Tree])((l, r) => Branch(l, r))

}