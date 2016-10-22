package net.joewing.jl.il

import net.joewing.jl.ObjectId

class InstructionId extends ObjectId
class ValueId extends ObjectId

sealed trait Instruction {
  val id: InstructionId

  def inputs: Set[ValueId] = Set()
  def outputs: Set[ValueId] = Set()
  final def values: Set[ValueId] = inputs ++ outputs
  def links: Set[InstructionId] = Set()
}

case class IntegerInstruction(id: InstructionId, dest: ValueId, value: Int) extends Instruction {
  override def toString = s"int($value)"
  override def outputs = Set(dest)
}

case class StringInstruction(id: InstructionId, dest: ValueId, value: String) extends Instruction {
  override def toString = s"string($value)"
  override def outputs = Set(dest)
}

case class BinaryInstruction(id: InstructionId, op: Operator, dest: ValueId, left: ValueId, right: ValueId) extends Instruction {
  override def toString = s"$dest <- $left $op $right"
  override def inputs = Set(left, right)
  override def outputs = Set(dest)
}

case class CallInstruction(id: InstructionId, target: InstructionId, dest: ValueId, args: List[ValueId]) extends Instruction {
  override def toString = s"$dest <- L$target(${args.map("V" + _.toString).mkString(",")})"
  override def inputs = args.toSet
  override def outputs = Set(dest)
}

case class ReturnInstruction(id: InstructionId, value: ValueId) extends Instruction {
  override def toString = s"return V$value"
  override def inputs = Set(value)
}

case class NativeInstruction(id: InstructionId, name: String, dest: ValueId, args: List[ValueId]) extends Instruction {
  override def toString = s"$dest <- $name(${args.map("V" + _.toString).mkString(",")})"
  override def inputs = args.toSet
  override def outputs = Set(dest)
}

case class JumpInstruction(id: InstructionId, target: InstructionId, cond: ValueId) extends Instruction {
  override def toString = s"if V$cond goto L$target"
  override def inputs = Set(cond)
}
