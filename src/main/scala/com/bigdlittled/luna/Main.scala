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
 
  val g = Graph[Any,DiEdge](
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
         Permission(Investment("USD Cash Returns", ReturnStream),User("Big Boss"),Read))

  // All the nodes
  println(g.nodes mkString ":")
 
  // All the holdings
  println(g.edges mkString ":")
  
  // The top level portfolio
  val p = g get Investment("Global 60/40", Portfolio)

 // Search for the portfolio (and get an Option)
  val q = g find Investment("Not there", Portfolio) getOrElse Investment("None", Portfolio)
  
  // All the nodes with a traverser 
  println(p.outerNodeTraverser.map(_.getClass.toString()))

  // All the nodes with a traverser 
  println(p.innerNodeTraverser.map(_.getClass.toString()))

    // All the nodes with a traverser 
  println(p.outerEdgeTraverser.map(_.getClass.toString()))

  // All the nodes with a traverser 
  println(p.innerEdgeTraverser.map(_.getClass.toString()))

    // All the nodes with a traverser 
  println(p.outerNodeTraverser.map(_.toString()))

  // Only the assets
  println(p.outerNodeTraverser.filter(AssetsOnly).map(_.toString()))
 
  // Only the return streams
  println(p.outerNodeTraverser.filter(ReturnStreamsOnly).map(_.toString()))

  // Only the portfolios
  println(p.outerNodeTraverser.filter(PortfoliosOnly).map(_.toString()))

    
  // Same thing with a for comprehension
  println(
    for {
      a <- p.outerNodeTraverser
      if PortfoliosOnly(a)
    } yield a.toString()
  )

  // Only the users
  println(p.outerNodeTraverser.filter(UsersOnly).map(_.toString()))

  val u = g get User("Big Boss")
 
  // Find all the portfolios that a user has some kind of access to
  println(u.diPredecessors.map(_.toString()))
 
  // And with a for comprehension
  // TODO: Figure out why the Set can't be filtered with filter(PortfoliosOnly)
  println(
    for {
      p <- g get User("Big Boss") diPredecessors;
      if PortfoliosOnly(p)
    } yield p.toString()
  )
  
  // Same thing with a one way traverser
  println((ArrayBuffer.empty[String] /: p.innerNodeTraverser)(_ += _.toString()).mkString)
 
  // And with an up down traverser
  println((ArrayBuffer.empty[String] /: p.innerNodeDownUpTraverser) {
    (buf, param) => param match {
      case (down, node) => 
        if (down) buf += (if (node eq p) "(" else "[") += node.toString() // Going down...
        else      buf += (if (node eq p) ")" else "]")                    // ...and up
    }
  }.mkString)

}
