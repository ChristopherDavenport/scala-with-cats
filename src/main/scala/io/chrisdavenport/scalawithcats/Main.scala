package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter11._
// import cats.implicits._
object Main {

  def main(args: Array[String]): Unit = {
    import FoldFun._
    // import cats.Eval
    import cats.Always
    import ListFun._
    val left = Foldable[List].foldLeft(List(1,2,3,4))(List.empty[Int]){
      case (acc, a) => a :: acc
    }
    println(left)

    val right = Foldable[List].foldRight(List(1,2,3,4))(Always(List.empty[Int])){
      case (a, acc) => acc.map(l => a :: l)
    }
    println(right.value)

    // println(FoldFun.ListFun.reverse(List(1,2,3,4)))
    // println(FoldFun.ListFun.reverse2(List(1,2,3,4)))
    // println(FoldFun.ListFun.listFoldable.foldMonoid(List(1,2,3,4))(identity))
    // println(FoldFun.ListFun.take(List(1,2,3,4), 5))
    // println(FoldFun.ListFun.take(List(1,2,3,4), 2))


  }

}