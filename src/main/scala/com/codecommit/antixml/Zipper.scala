package com.codecommit.antixml

trait Zipper extends NodeSeq { self =>
  private type NSZip = NodeSeq with Zipper
  
  val path = List[NodeSeq => NSZip]()
  
  // er...maybe this is a bad method name
  def stripZipper = new NodeSeq(toVector)
  
  def up: NSZip = path match {
    // TODO this is stupid, I'm just trying to avoid a recursive type for now
    case rebuild :: remainder => new NodeSeq(rebuild(this).toVector) with Zipper {
      override val path = remainder
    }
    
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
