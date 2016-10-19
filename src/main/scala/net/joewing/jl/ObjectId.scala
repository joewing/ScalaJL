package net.joewing.jl

abstract class ObjectId extends Ordered[ObjectId] {
  private val id = ObjectId.getNext
  override def compare(that: ObjectId): Int = id - that.id
  override def toString = id.toString
}

object ObjectId {
  private var next = 0
  private def getNext: Int = {
    val result = next
    next += 1
    result
  }
}
