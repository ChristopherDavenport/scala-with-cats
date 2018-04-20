package io.chrisdavenport.scalawithcats.chapter11

object GCounterExercises {
  final case class GCounter(counters: Map[String, Int]) { 

    def increment(machine: String, amount: Int): GCounter = {
      val updateOrAdd : (String, Int) = counters.get(machine).fold(
        (machine, amount)
      )(value => (machine , amount + value))

      GCounter(counters + updateOrAdd)
    }
    def merge(that: GCounter): GCounter = {
      val eventualMap : scala.collection.mutable.Map[String, Int] = 
        scala.collection.mutable.Map[String, Int]()

      
      counters.map{ case (k, v)=> eventualMap += (k -> v)}

      that.counters.map{case (k,vThat) => 
        counters.get(k).fold(
          eventualMap += (k ->  vThat)
        )(
          vThis => {
            if (vThis >= vThat) eventualMap += (k -> vThis)
            else eventualMap += (k -> vThat)
          } 
        )
      }

      GCounter(eventualMap.toMap) 
    }
    def total: Int = counters.values.sum
  }
}