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

    val investments = Graph[Any,DiEdge](
      Holding(Investment("Global 60/40", Portfolio), Investment("Equities"), 0.5),
        Holding(Investment("Equities"), Investment("US Equities"), 0.6),
          Holding(Investment("US Equities"), Investment("US Equities Excess Returns", ReturnStream), 1.0),
          Holding(Investment("US Equities"), Investment("USD Cash Returns", ReturnStream), 1.0),
        Holding(Investment("Equities"), Investment("EUR Equities"), 0.4),
          Holding(Investment("EUR Equities"), Investment("EUR Equities Excess Returns", ReturnStream), 1.0),
          Holding(Investment("EUR Equities"), Investment("EUR Cash Returns", ReturnStream), 1.0),
      Holding(Investment("Global 60/40", Portfolio), Investment("Bonds"), 0.5),
        Holding(Investment("Bonds"), Investment("Nominal Bonds"), 0.7),
          Holding(Investment("Nominal Bonds"), Investment("Nominal Bonds Excess Returns", ReturnStream), 1.0),
          Holding(Investment("Nominal Bonds"), Investment("USD Cash Returns", ReturnStream), 1.0),
        Holding(Investment("Bonds"), Investment("IL Bonds"), 0.3),
          Holding(Investment("IL Bonds"), Investment("IL Bonds Excess Returns", ReturnStream), 1.0),
          Holding(Investment("IL Bonds"), Investment("USD Cash Returns", ReturnStream), 1.0)
  )

  val permissions = investments.nodes.map(_.toOuter).map(Permission(_,User("Big Boss"),Read))
 
  val portfolio = investments ++ permissions
  
  // All the nodes
  println(portfolio.nodes mkString ":")
 
  // All the holdings
  println(portfolio.edges mkString ":")
  
  // The top level portfolio
  val p = portfolio get Investment("Global 60/40", Portfolio)

 // Search for the portfolio (and get an Option)
  val q = portfolio find Investment("Not there", Portfolio) getOrElse Investment("None", Portfolio)
  
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

  val u = portfolio get User("Big Boss")
 
  // Find all the portfolios that a user has some kind of access to
  println(u.diPredecessors.map(_.toString()))
 
  // And with a for comprehension
  println(
    for {
      o <- portfolio.get(User("Big Boss")).diPredecessors
      if PortfoliosOnly(o.toOuter)
    } yield o.toString()
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
