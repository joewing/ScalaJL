package net.joewing.jl.il

import net.joewing.jl.BaseResult
import net.joewing.jl.functions.SpecialFunction

class ProgramId

class Program(val id: ProgramId, val instructions: List[Instruction]) extends BaseResult[Program] {

  def this(instructions: List[Instruction]) = this(new ProgramId(), instructions)

  override def create(func: SpecialFunction): Program = func.generate

  def append(i: Instruction): Program = new Program(id, instructions :+ i)

}