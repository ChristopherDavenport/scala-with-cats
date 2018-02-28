package io.chrisdavenport.scalawithcats.chapter4

import cats.implicits._
import cats.data._
// import cats._

object PostOrderCalc {

  sealed trait Op
  case object Add extends Op
  case object Sub extends Op
  case object Mult extends Op
  case object Div extends Op
  final case class Num(value: Int) extends Op

  sealed trait ParseFailure
  case class BadOpString(badString: String) extends ParseFailure
  case object BadStackState extends ParseFailure 

  // Non Referentially Transparent Bad
  def toOp(sym: String): Either[ParseFailure, Op] = {
    sym match {
      case "+" => Either.right(Add)
      case "-" => Either.right(Sub)
      case "*" => Either.right(Mult)
      case "/" => Either.right(Div)
      case other => 
        Either.catchNonFatal(other.toInt).fold(
          t => Either.left(BadOpString(other)),
          i => Either.right(Num(i))
        )
    }
  }
  // def applyCombinerF[F[_]](implicit F: MonadError[F, Throwable]): StateT[F, List[Op], Num] = 

  // Also Not Good!
  def applyCombiner(f: (Num, Num) => Num): State[Either[ParseFailure, List[Op]], Option[Num]] = State[Either[ParseFailure, List[Op]], Option[Num]]{
    case Right(Num(x) :: Num(y) :: xs) => (Either.right(xs), f(Num(x), Num(y)).some)
    case Left(e) => (Either.left(e), None)
    case _ => (Either.left(BadStackState), None)
  }

  def applyNum(n: Num): State[Either[ParseFailure, List[Op]], Unit] = State.modify[Either[ParseFailure, List[Op]]]{
    case l@Left(_) => l
    case Right(list) => Either.right(n :: list)
  }
  

  def evalOne(sym: String): State[Either[ParseFailure,List[Op]], Unit] = toOp(sym) match {
    case Right(Add) => applyCombiner((x, y) => Num(x.value + y.value))
    .flatMap(_.fold(().pure[State[Either[ParseFailure, List[Op]], ?]])(applyNum))
    case Right(Sub) => applyCombiner((x, y) => Num(x.value - y.value))
    .flatMap(_.fold(().pure[State[Either[ParseFailure, List[Op]], ?]])(applyNum))
    case Right(Mult) => applyCombiner((x, y) => Num(x.value * y.value))
    .flatMap(_.fold(().pure[State[Either[ParseFailure, List[Op]], ?]])(applyNum))
    case Right(Div) => applyCombiner((x, y) => Num(x.value / y.value))
    .flatMap(_.fold(().pure[State[Either[ParseFailure, List[Op]], ?]])(applyNum))
    case Right(n@Num(_)) => applyNum(n)
    case Left(e) => State.set[Either[ParseFailure, List[Op]]](Either.left(e))
  }

  def test1(): Unit = {
    val result = evalOne("42").get.runA(Either.right(Nil)).value
    println(result)
  }
  def test2(): Unit = {
    val prog = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      _ <- evalOne("+")
      ans <- State.get
    } yield ans

    val result = prog.runA(Either.right(Nil)).value
    println(result)
  }

  def evalAll(l: List[String]): State[Either[ParseFailure, List[Op]], Unit] = 
    l.foldLeft(().pure[State[Either[ParseFailure, List[Op]], ?]])(_ *> evalOne(_))

  def test3(): Unit = {
    val prog = evalAll(List("1", "2", "+", "3", "*")) *> State.get
    val result = prog.runA(Either.right(Nil)).value
    println(result)
  }

  def test4(): Unit = {
    val prog = for {
      _   <- evalAll(List("1", "2", "+"))
      _   <- evalAll(List("3", "4", "+"))
      _ <- evalOne("*")
      ans <- State.get
    } yield ans
    val result = prog.runA(Either.right(Nil)).value
    println(result)

  }

  def test5(): Unit = {
    val prog = evalOne("Yellow") *> State.get
    val result = prog.runA(Either.right(Nil)).value
    println(result)
  }

  def test6(): Unit = {
    val prog = evalAll(List("1", "+")) *> State.get
    val result = prog.runA(Either.right(Nil)).value
    println(result)
  }

  def test7(): Unit = {
    val prog = evalAll(List("1", "2", "3", "+", "*")) *> State.get
    val result = prog.runA(Either.right(Nil)).value
    println(result)
  }




}