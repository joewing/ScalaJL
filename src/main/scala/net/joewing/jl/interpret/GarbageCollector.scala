package net.joewing.jl.interpret

import net.joewing.jl.{LambdaResult, ScopeId}

private class GarbageCollector(private[this] val scopes: Map[ScopeId, ValueResult]) {

  private[this] def getReferences(id: ScopeId): Set[ScopeId] = {
    scopes(id) match {
      case LambdaResult(stack) => getReferences(stack)
      case _ => Set()
    }
  }

  private[this] def getReferences(lst: Traversable[ScopeId]): Set[ScopeId] = {
    lst.flatMap(getReferences).toSet
  }

  private def run(marked: Traversable[ScopeId]): Map[ScopeId, ValueResult] = {
    val referencedScopes = getReferences(marked)
    val unusedScopes = scopes.keySet -- referencedScopes
    scopes -- unusedScopes
  }
}

object GarbageCollector {
  def run(stack: Traversable[ScopeId], scopes: Map[ScopeId, ValueResult]): Map[ScopeId, ValueResult] = {
    new GarbageCollector(scopes).run(stack)
  }
}