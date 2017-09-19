package com.bigdlittled.luna

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer
import domain._

case class Investment(name: String)
case class User(name: String)

case class Holding[+N](fromNode: N, toNode: N)
  extends DiEdge[N](NodeProduct(fromNode, toNode))
  with    ExtendedKey[N]
  with    EdgeCopy[Holding]
  with    OuterEdge[N,Holding] 
{
  private def this(nodes: Product) {
    this(nodes.productElement(0).asInstanceOf[N],
         nodes.productElement(1).asInstanceOf[N])
  }
  def keyAttributes = Seq()
  override def copy[NN](newNodes: Product) = new Holding[NN](newNodes)
  override protected def attributesToString = s"Holding" 
}

case class Permission[+N](fromNode: N, toNode: N, something: Int)
  extends DiEdge[N](NodeProduct(fromNode, toNode))
  with    ExtendedKey[N]
  with    EdgeCopy[Permission]
  with    OuterEdge[N,Permission]
{
  private def this(nodes: Product, something: Int) {
    this(nodes.productElement(0).asInstanceOf[N],
         nodes.productElement(1).asInstanceOf[N], something)
  }
  def keyAttributes = Seq()
  override def copy[NN](newNodes: Product) = new Permission[NN](newNodes, something)
  override protected def attributesToString = s"Permission" 
}

object ED {
  implicit final class ImplicitEdge[Investment](val e: DiEdge[Investment]) extends AnyVal {
    def ## (something: String) = new Holding[Investment](e.source, e.target)
  } 
}

object Main extends App {
  //import ED._

  val g = Graph(
    Investment("One")~>Investment("Two"),
      Holding(Investment("Two"),Investment("Three")),
    Permission(Investment("One"),User("Four"),2),
      Holding(Investment("Four"),User("Five")),
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
