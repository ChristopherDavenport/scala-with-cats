package io.chrisdavenport.scalawithcats.chapter1

trait Printable[A]{
  def print(a: A): String
}

object Printable {
  def apply[A](implicit ev: Printable[A]): Printable[A] = ev

  def fromPrint[A](f: A => String): Printable[A] = new Printable[A]{
    def print(a: A): String = f(a)
  }

  def format[A: Printable](a: A): String = Printable[A].print(a)

}

object PrintableInstances {

  implicit val intPrintable = Printable.fromPrint[Int](_.toString)
  implicit val stringPrintable = Printable.fromPrint[String](identity)


}

object PrintableSyntax {

  implicit class PrintableSynt[A](a: A){
    def print(implicit ev: Printable[A]): String = ev.print(a)
  }

}

object PrintableConversion {
  implicit def print[A](a: A)(implicit ev: Printable[A]): String = ev.print(a)
}

object ExampleClass {
  final case class Cat(name: String, age: Int, color: String)
  object Cat {
    def catPrintable(implicit S: Printable[String], I: Printable[Int]) = 
    Printable.fromPrint[Cat](cat => s"Cat(${S.print(cat.name)},${I.print(cat.age)},${S.print(cat.color)})")
  }

}