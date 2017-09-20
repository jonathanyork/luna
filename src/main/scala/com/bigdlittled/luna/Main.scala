package com.bigdlittled.luna

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer
import domain._

object Main extends App {
 import Permission._
 import Taxonomy._
 import Filters._
 
  val g = Graph(
   Holding(Investment("Global 60/40", Portfolio), Investment("Equities"), 0.5),
       Permission(Investment("Equities"),User("Big Boss"),Read),
     Holding(Investment("Equities"), Investment("US Equities"), 0.6),
       Permission(Investment("US Equities"),User("Big Boss"),Read),
       Holding(Investment("US Equities"), Investment("US Equities Excess Returns", ReturnStream), 1.0),
         Permission(Investment("US Equities Excess Returns", ReturnStream),User("Big Boss"),Read),
       Holding(Investment("US Equities"), Investment("USD Cash Returns", ReturnStream), 1.0),
         Permission(Investment("USD Cash Returns", ReturnStream),User("Big Boss"),Read),
     Holding(Investment("Equities"), Investment("EUR Equities"), 0.4),
       Permission(Investment("EUR Equities"),User("Big Boss"),Read),
       Holding(Investment("EUR Equities"), Investment("EUR Equities Excess Returns", ReturnStream), 1.0),
         Permission(Investment("EUR Equities Excess Returns", ReturnStream),User("Big Boss"),Read),
       Holding(Investment("EUR Equities"), Investment("EUR Cash Returns", ReturnStream), 1.0),
         Permission(Investment("EUR Cash Returns", ReturnStream),User("Big Boss"),Read),
   Holding(Investment("Global 60/40", Portfolio), Investment("Bonds"), 0.5),
     Permission(Investment("Bonds"),User("Big Boss"),Read),
     Holding(Investment("Bonds"), Investment("Nominal Bonds"), 0.7),
       Permission(Investment("Nominal Bonds"),User("Big Boss"),Read),
       Holding(Investment("Nominal Bonds"), Investment("Nominal Bonds Excess Returns", ReturnStream), 1.0),
         Permission(Investment("Nominal Bonds Excess Returns", ReturnStream),User("Big Boss"),Read),
       Holding(Investment("Nominal Bonds"), Investment("USD Cash Returns", ReturnStream), 1.0),
         Permission(Investment("USD Cash Returns"),User("Big Boss"),Read),
     Holding(Investment("Bonds"), Investment("IL Bonds"), 0.3),
       Permission(Investment("IL Bonds"),User("Big Boss"),Read),
       Holding(Investment("IL Bonds"), Investment("IL Bonds Excess Returns", ReturnStream), 1.0),
         Permission(Investment("IL Bonds Excess Returns", ReturnStream),User("Big Boss"),Read),
       Holding(Investment("IL Bonds"), Investment("USD Cash Returns", ReturnStream), 1.0),
         Permission(Investment("USD Cash Returns", ReturnStream),User("Big Boss"),Read),
       
  //     Holding(Investment("Two"),Investment("Three"), 0.6),
  //  Permission(Investment("One"),User("Four"),Read),
  //    Holding(Investment("Four"),User("Five"), 0.4),
    (Investment("One")~+>Investment("Six"))(0.5)
  )

  // All the nodes
  println(g.nodes mkString ":")
 
  // All the holdings
  println(g.edges mkString ":")

  val g2 = g + (Investment("One")~+>Investment("Six"))(0.2)
  // All the nodes
  println(g2.edges mkString ":")
  
  // The top level portfolio
  val p = g get Investment("One")

  val q = g find Investment("One")
  
  // All the nodes with a traverser 
  println(p.outerNodeTraverser.map(_.getClass.toString()))

  // All the nodes with a traverser 
  println(p.innerNodeTraverser.map(_.getClass.toString()))

    // All the nodes with a traverser 
  println(p.outerEdgeTraverser.map(_.getClass.toString()))

  // All the nodes with a traverser 
  println(p.innerEdgeTraverser.map(_.getClass.toString()))

  // Same thing with a for comprehension
  println(
    for {
      a <- p.outerNodeTraverser
    } yield a.toString()
  )
}
