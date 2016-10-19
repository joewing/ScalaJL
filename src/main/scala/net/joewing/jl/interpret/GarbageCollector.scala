package net.joewing.jl.interpret

import net.joewing.jl.{Scope, ScopeId}

private class GarbageCollector(private[this] val scopes: Map[ScopeId, InterpreterScope]) {

  private[this] def getReferences(id: ScopeId, visited: Set[ScopeId]): Set[ScopeId] = {
    if (!visited.contains(id)) {
      val newScopes = scopes(id).values.values.collect { case LambdaValueResult(stack, _, _) => stack }.flatten.toSet
      newScopes.foldLeft(visited + id) { (scopeSet, newId) => getReferences(newId, scopeSet) }
    } else {
      visited
    }
  }

  private[this] def getReferences(lst: Traversable[ScopeId]): Set[ScopeId] = {
    lst.foldLeft(Set[ScopeId]()) { (scopeSet, id) => getReferences(id, scopeSet) }
  }

  private def run(marked: Traversable[ScopeId]): Map[ScopeId, InterpreterScope] = {
    val referencedScopes = getReferences(marked)
    scopes.filterKeys(referencedScopes.contains)
  }
}

object GarbageCollector {
  def run(stack: Traversable[ScopeId], scopes: Map[ScopeId, InterpreterScope]): Map[ScopeId, InterpreterScope] = {
    new GarbageCollector(scopes).run(stack)
  }
}