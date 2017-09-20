package com.bigdlittled.luna.domain

import scalaz._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
import scala.collection.mutable.ArrayBuffer

case class Investment(name: String, investmentType: Taxonomy.InvestmentType = Taxonomy.Asset)

case class Holding[+N](fromNode: N, toNode: N, amount: Double)
  extends DiEdge[N](NodeProduct(fromNode, toNode))
  with    ExtendedKey[N]
  with    EdgeCopy[Holding]
  with    OuterEdge[N,Holding] 
{
  private def this(nodes: Product, amount: Double) {
    this(nodes.productElement(0).asInstanceOf[N],
         nodes.productElement(1).asInstanceOf[N], amount)
  }
  def keyAttributes = Seq() // Amount isn't a key attribute
  override def copy[NN](newNodes: Product) = new Holding[NN](newNodes, amount)
  override protected def attributesToString = s"Holding" 
}

/*
 * Here I'm typing though a taxonomy. In reality I'd probably want to use
 * either a subtype or composition in this case but it illustrates the
 * mechanism effectively.
 */
object Taxonomy {
  sealed trait InvestmentType
  case object Portfolio extends InvestmentType
  case object Asset extends InvestmentType
  case object ReturnStream extends InvestmentType
  val types = Seq(Portfolio, Asset, ReturnStream)
}

/*
 * Here's I've used case objects to define a set of standard filters. Obviously they
 * could be more elaborate and they can be composed with boolean operators.
 */
object Filters {
  case object AssetsOnly extends ((Any) => Boolean) {
    def apply(a: Any) = a match {
      case a: Investment => a.investmentType == Taxonomy.Asset
      case _             => false
    }
  }
  case object ReturnStreamsOnly extends ((Any) => Boolean) {
    def apply(a: Any) = a match {
      case a: Investment => a.investmentType == Taxonomy.ReturnStream
      case _             => false
    }
  }
  case object PortfoliosOnly extends ((Any) => Boolean) {
    def apply(a: Any) = a match {
      case a: Investment => a.investmentType == Taxonomy.Portfolio
      case _             => false
    }
  }
  case object UsersOnly extends ((Any) => Boolean) {
    def apply(a: Any) = a match {
      case a: User => true
      case _       => false
    }
  }
 }