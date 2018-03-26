package io.chrisdavenport.scalawithcats.chapter7

object ReflectingOnFolds {
  def concatFoldLeft[A](l: List[A]): List[A] = l.foldLeft(List.empty[A])((l, a) => a :: l)
  // @ List(1,2,3).foldLeft(List.empty[Int])((l, a) => a :: l)
  // res0: List[Int] = List(3, 2, 1)

  def concatFoldRight[A](l: List[A]): List[A] = l.foldRight(List.empty[A])((a, l) => a :: l)
  // @ List(1,2,3).foldRight(List.empty[Int])((a, l) => a :: l)
  // res1: List[Int] = List(1, 2, 3)

}