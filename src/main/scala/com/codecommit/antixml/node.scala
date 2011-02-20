package com.codecommit.antixml

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq

sealed trait Node

case class ProcInstr(target: String, data: String) extends Node

case class Elem(ns: Option[String], name: String, attrs: Map[String, String], children: Group[Node]) extends Node {
  def \[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbf: CanBuildFrom[Group[Elem], B, That]): That = {
    Group(this) \ selector
  }

  def \\[B, That <: IndexedSeq[B]](selector: Selector[B, That])(implicit cbf: CanBuildFrom[Traversable[_], B, That]): That = {
    Group(this) \\ selector
  }

  override def toString = {
    val prefix = ns map { _ + ':' } getOrElse ""
    val qName = prefix + name
    
    val attrStr = if (attrs.isEmpty) 
      ""
    else
      " " + (attrs map { case (key, value) => key + "=\"" + value + '"' } mkString " ")
    
    val partial = "<" + qName + attrStr
    if (children.isEmpty)
      partial + "/>"
    else
      partial + '>' + children.toString + "</" + qName + '>'
  }
}

case class Text(text: String) extends Node {
  override def toString = text
}

case class Whitespace(text: String) extends Node {
  override def toString = text
}
