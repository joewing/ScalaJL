package net.joewing.jl.interpret

import net.joewing.jl._

class InterpreterContext(
    val stack: List[Int],
    _scopes: Map[Int, Scope[ValueResult, InterpreterContext]],
    _currentScope: Int)
  extends Context[ValueResult, InterpreterContext](_scopes, _currentScope) {

  override protected def create(
      newScopes: Map[Int, Scope[ValueResult, InterpreterContext]],
      newId: Int): InterpreterContext = {
    new InterpreterContext(stack, newScopes, newId)
  }

  private def hasReference(id: Int): Boolean = {
    scopes.values.exists { scope =>
      scope.values.values.exists {
        case LambdaValueResult(sid, _, _) => sid == id
        case _ => false
      }
    }
  }

  // Enter a new dynamic scope rooted at root.
  def enterScope(root: Int): InterpreterContext = {
    val newScopeId = nextScopeId
    val scope = Scope[ValueResult, InterpreterContext](newScopeId, root, Map())
    new InterpreterContext(currentScope +: stack, scopes + (newScopeId -> scope), newScopeId)
  }

  // Leave the current dynamic scope.
  def leaveScope: InterpreterContext = {
    val newScopes = if (!hasReference(stack.head)) scopes - stack.head else scopes
    new InterpreterContext(stack.tail, newScopes, stack.head)
  }

}