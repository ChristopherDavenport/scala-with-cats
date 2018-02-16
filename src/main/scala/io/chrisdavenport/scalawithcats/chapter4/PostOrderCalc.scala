package io.chrisdavenport.scalawithcats.chapter4

import cats.implicits._
import cats.data.State

object PostOrderCalc {

  sealed trait Op
  case object Add extends Op
  case object Sub extends Op
  case object Mult extends Op
  case object Div extends Op
  final case class Num(value: Int) extends Op


  // Non Referentially Transparent Bad
  def toOp(sym: String): Op = {
    sym match {
      case "+" => Add
      case "-" => Sub
      case "*" => Mult
      case "/" => Div
      case other => Num(other.toInt)
    }
  }

  // Also Not Good!
  def applyCombiner(f: (Num, Num) => Num): State[List[Op], Num] = State[List[Op], Num]{
    case Num(x) :: Num(y) :: xs => 
      (xs, f(Num(x), Num(y)))
    case _ => throw new Throwable("Ack!")
  }

  def applyNum(n: Num): State[List[Op], Unit] = State.modify[List[Op]](n :: _)
  

  def evalOne(sym: String): State[List[Op], Unit] = toOp(sym) match {
    case Add => applyCombiner((x, y) => Num(x.value + y.value)).flatMap(applyNum)
    case Sub => applyCombiner((x, y) => Num(x.value - y.value)).flatMap(applyNum)
    case Mult => applyCombiner((x, y) => Num(x.value * y.value)).flatMap(applyNum)
    case Div => applyCombiner((x, y) => Num(x.value / y.value)).flatMap(applyNum)
    case n@Num(_) => applyNum(n)
  }

  def test1(): Unit = {
    val result = evalOne("42").get.runA(Nil).value
    println(result)
  }
  def test2(): Unit = {
    val prog = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      _ <- evalOne("+")
      ans <- State.get
    } yield ans

    val result = prog.runA(Nil).value
    println(result)
  }

  def evalAll(l: List[String]): State[List[Op], Unit] = l.foldLeft(().pure[State[List[Op], ?]])(_ *> evalOne(_))

  def test3(): Unit = {
    val prog = evalAll(List("1", "2", "+", "3", "*")) *> State.get
    val result = prog.runA(Nil).value
    println(result)
  }

  def test4(): Unit = {
    val prog = for {
      _   <- evalAll(List("1", "2", "+"))
      _   <- evalAll(List("3", "4", "+"))
      _ <- evalOne("*")
      ans <- State.get
    } yield ans
    val result = prog.runA(Nil).value
    println(result)

  }




}