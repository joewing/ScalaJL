package net.joewing.jl.il

import net.joewing.jl._

class ILContext(
    val program: Program,
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, ILScope]
  ) extends Context[ValueId, ILScope, ILContext](_stack, _scopes) {

  protected def create(stack: List[ScopeId], scopes: Map[ScopeId, ILScope]): ILContext =
    new ILContext(new Program(new InstructionId, List(), Map(), Map()), stack, scopes)

  def newScope: ILScope = new ILScope(new ScopeId, Map())

  def newValue(t: ValueType): (ILContext, ValueId) = {
    val (program, value) = program.newValue(t)
    (new ILContext(program, stack, scopes), value)
  }

  def addInstruction(instruction: Instruction): ILContext = {
    new ILContext(program.append(instruction), stack, scopes)
  }

}