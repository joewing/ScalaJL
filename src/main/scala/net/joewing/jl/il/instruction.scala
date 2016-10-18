package net.joewing.jl.il

sealed abstract class Instruction {
  def program: Program = new Program(List(this))
}

case class IntegerInstruction(value: Int) extends Instruction

case class StringInstruction(value: String) extends Instruction

case class AddInstruction(left: ProgramId, right: ProgramId) extends Instruction

case class SubInstruction(left: ProgramId, right: ProgramId) extends Instruction

case class MulInstruction(left: ProgramId, right: ProgramId) extends Instruction

case class DivInstruction(left: ProgramId, right: ProgramId) extends Instruction

case class CallInstruction(target: ProgramId, args: List[ProgramId]) extends Instruction

case class JumpInstruction(cond: ProgramId, target: ProgramId) extends Instruction
