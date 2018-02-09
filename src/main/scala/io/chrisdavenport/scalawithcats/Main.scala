package io.chrisdavenport.scalawithcats

import chapter3._
import Printable._

object Main {

  def main(args: Array[String]): Unit = {
    println(true.format)
    println(Box("Yellow").format)
    println(Box(true).format)
    
  }

}