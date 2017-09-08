package com.bigdlittled.luna

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer
import domain._

trait D { def name: String }
case class A(name: String)
case class B(name: String)
case class C(name: String) extends D

trait ED

case class EA[+N](fromNode: N, toNode: N)
  extends DiEdge[N](NodeProduct(fromNode, toNode))
  with    ExtendedKey[N]
  with    EdgeCopy[EA]
  with    OuterEdge[N,EA] 
  with    ED
{
  private def this(nodes: Product) {
    this(nodes.productElement(0).asInstanceOf[N],
         nodes.productElement(1).asInstanceOf[N])
  }
  def keyAttributes = Seq()
  override def copy[NN](newNodes: Product) = new EA[NN](newNodes)
  override protected def attributesToString = s"EA" 
}

case class EB[+N](fromNode: N, toNode: N)
  extends DiEdge[N](NodeProduct(fromNode, toNode))
  with    ExtendedKey[N]
  with    EdgeCopy[EB]
  with    OuterEdge[N,EB]
  with    ED
{
  private def this(nodes: Product) {
    this(nodes.productElement(0).asInstanceOf[N],
         nodes.productElement(1).asInstanceOf[N])
  }
  def keyAttributes = Seq()
  override def copy[NN](newNodes: Product) = new EB[NN](newNodes)
  override protected def attributesToString = s"EB" 
}

object ED {
  implicit final class ImplicitEdge[A <: D](val e: DiEdge[A]) extends AnyVal {
    def ## (something: String) = new EA[A](e.source, e.target)
  } 
}

object Main extends App {
  //import ED._

  val g = Graph(
    A("One")~>A("Two"),
      EA(A("Two"),A("Three")),
    EB(A("One"),C("Four")),
      EA(A("Four"),B("Five")),
    (A("One")~+>A("Six"))(0.5)
  )

  // All the nodes
  println(g.nodes mkString ":")
 
  // All the holdings
  println(g.edges mkString ":")

  val g2 = g + (A("One")~+>A("Six"))(0.2)
  // All the nodes
  println(g2.edges mkString ":")
  
  // The top level portfolio
  val p = g get A("One")
 
  // All the nodes with a traverser 
  println(p.outerNodeTraverser.map(_.getClass.toString()))

  // All the nodes with a traverser 
  println(p.innerNodeTraverser.map(_.getClass.toString()))

    // All the nodes with a traverser 
  println(p.outerEdgeTraverser.map(_.getClass.toString()))

  // All the nodes with a traverser 
  println(p.innerEdgeTraverser.map(_.getClass.toString()))

/*
  // Same thing with a for comprehension
  println(
    for {
      a <- p.outerNodeTraverser
    } yield a.toString()
  )
*/
}
