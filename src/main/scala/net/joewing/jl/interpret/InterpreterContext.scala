package net.joewing.jl.interpret

import net.joewing.jl.{Context, Scope, ScopeId}

class InterpreterContext(
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, Scope[ValueResult]])
  extends Context[ValueResult, InterpreterContext](_stack, _scopes) {

  override def create(
      stack: List[ScopeId],
      scopes: Map[ScopeId, Scope[ValueResult]]): InterpreterContext = {
    new InterpreterContext(stack, scopes)
  }

  def pushScope(localStack: List[ScopeId]): InterpreterContext =
    new InterpreterContext(localStack ++ stack, scopes)

  def popScope(localStack: List[ScopeId]): InterpreterContext =
    new InterpreterContext(stack.drop(localStack.length), scopes)

}