package com.codecommit.antixml

trait Zipper extends NodeSeq { self =>
  private type NSZip = NodeSeq with Zipper
  
  val path = List[NodeSeq => NSZip]()
  
  // er...maybe this is a bad method name
  def stripZipper = new NodeSeq(toVector)
  
  def up = path match {
    case hd :: tail => hd(this)
    case Nil => error("Up at top")
  }
  
  override def updated(index: Int, node: Node): NSZip = {
    new NodeSeq(super.updated(index, node).toVector) with Zipper {
      override val path = self.path
    }
  }
  
  // TODO more functions
  
  override def \(name: String) = search(name, path)
}
