package com.codecommit.antixml

import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}

class NodeSeq protected (private val nodes: Vector[Node]) extends IndexedSeq[Node] 
    with IndexedSeqLike[Node, NodeSeq] {
  
  override def newBuilder = NodeSeq.newBuilder
  
  def length = nodes.length
  
  def apply(i: Int) = nodes(i)
  
  def +:(node: Node) = new NodeSeq(node +: nodes)
  
  def :+(node: Node) = new NodeSeq(nodes :+ node)
  
  def ++(that: NodeSeq) = new NodeSeq(this.nodes ++ that.nodes)
  
  override def drop(n: Int) = new NodeSeq(nodes drop n)
  
  override def dropRight(n: Int) = new NodeSeq(nodes dropRight n)
  
  override def head = nodes.head
  
  override def init = new NodeSeq(nodes.init)
  
  override def iterator = nodes.iterator
  
  override def last = nodes.last
  
  override def lengthCompare(len: Int) = nodes lengthCompare len
  
  override def reverseIterator = nodes.reverseIterator
  
  override def slice(from: Int, until: Int) = new NodeSeq(nodes.slice(from, until))
  
  override def splitAt(n: Int) = {
    val (left, right) = nodes splitAt n
    (new NodeSeq(left), new NodeSeq(right))
  }
  
  override def tail = new NodeSeq(nodes.tail)
  
  override def take(n: Int) = new NodeSeq(nodes take n)
  
  override def takeRight(n: Int) = new NodeSeq(nodes takeRight n)
  
  def updated(index: Int, node: Node) = new NodeSeq(nodes.updated(index, node))
  
  def \(name: String): NodeSeq with Zipper = search(name, Nil)
  
  // TODO optimize
  protected def search(name: String, pathToSelf: List[NodeSeq => (NodeSeq with Zipper)]): NodeSeq with Zipper = {
    val results = nodes map {
      case e @ Elem(_, _, _, children) => {
        val selectedWithIndexes = children.zipWithIndex flatMap {
          case (e @ Elem(_, `name`, _, _), i) => Some(e -> i)
          case _ => None
        }
        
        val indexes = selectedWithIndexes map { case (_, i) => i }
        val selected = selectedWithIndexes map { case (e, _) => e }
        
        def rebuild(children2: NodeSeq) = {
          val revisedChildren = (indexes zip children2).foldLeft(children) {
            case (vec, (i, e)) => vec.updated(i, e)
          }
          e.copy(children=revisedChildren)
        }
        
        Some((selected, rebuild _))
      }
      
      case _ => None
    }
    
    val (_, map) = results.foldLeft((0, Vector[(Int, Int, NodeSeq => Node)]())) {
      case ((i, acc), Some((res, f))) if !res.isEmpty =>
        (i + res.length, acc :+ (i, i + res.length, f))
      
      case ((i, acc), _) => (i, acc)
    }
    
    def rebuild(aggregate: NodeSeq): NodeSeq with Zipper = {
      val (_, nodes2) = map.foldLeft((0, nodes)) {
        case ((i, nodes), (start, end, f)) => {
          val nodes2 = nodes(i) match {
            case _: Elem =>
              nodes.updated(i, f(aggregate.slice(start, end)))
            
            case _ => nodes
          }
          (i + 1, nodes2)
        }
      }
      
      new NodeSeq(nodes2) with Zipper
    }
    
    val cat = results flatMap {
      case Some((selected, _)) => selected
      case None => Vector()
    }
    
    new NodeSeq(cat) with Zipper {
      override val path = (rebuild _) :: pathToSelf
    }
  }
  
  // TODO optimize
  def \\(name: String): NodeSeq = {
    val recursive = this flatMap {
      case Elem(_, _, _, children) => children \\ name
      case _ => Nil
    }
    
    (this \ name stripZipper) ++ recursive
  }
  
  def toVector = nodes
  
  override def toString = nodes.mkString
}

object NodeSeq extends ((Node*) => NodeSeq) {
  implicit def canBuildFrom: CanBuildFrom[NodeSeq, Node, NodeSeq] = new CanBuildFrom[NodeSeq, Node, NodeSeq] {
    def apply(coll: NodeSeq) = newBuilder
    def apply() = newBuilder
  }
  
  def newBuilder = new VectorBuilder[Node] mapResult { new NodeSeq(_) }

  def empty = new NodeSeq(Vector.empty)
  
  def apply(nodes: Node*) = fromSeq(nodes)
  
  def fromSeq(seq: Seq[Node]) = seq match {
    case vec: Vector[Node] => new NodeSeq(vec)
    case _ => new NodeSeq(Vector(seq: _*))
  }
}
