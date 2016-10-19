package net.joewing.jl.interpret

import net.joewing.jl.{Context, Scope, ScopeId}

class InterpreterContext(
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, InterpreterScope])
  extends Context[ValueResult, InterpreterScope, InterpreterContext](_stack, _scopes) {

  protected def create(stack: List[ScopeId], scopes: Map[ScopeId, InterpreterScope]): InterpreterContext = {
    new InterpreterContext(stack, scopes)
  }

  def newScope: InterpreterScope = new InterpreterScope(new ScopeId(), Map())

  def pushScope(localStack: List[ScopeId]): InterpreterContext =
    new InterpreterContext(localStack ++ stack, scopes)

  def popScope(localStack: List[ScopeId]): InterpreterContext =
    new InterpreterContext(stack.drop(localStack.length), scopes)

}