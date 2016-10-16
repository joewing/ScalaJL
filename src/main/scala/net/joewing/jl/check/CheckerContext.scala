package net.joewing.jl.check

import net.joewing.jl.{Context, Scope, ScopeId}

class CheckerContext(
    private val equivalences: Map[TypeId, TypeId],
    private val bounds: Map[TypeId, TypeResult],
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, Scope[TypeResult]])
  extends Context[TypeResult, CheckerContext](_stack, _scopes) {

  override protected def create(
      stack: List[ScopeId],
      scopes: Map[ScopeId, Scope[TypeResult]]): CheckerContext = {
    new CheckerContext(equivalences, bounds, stack, scopes)
  }

  def addEquivalence(a: TypeResult, b: TypeResult): CheckerContext = (a, b) match {
    case (UnknownTypeResult(id1), UnknownTypeResult(id2)) => addEquivalence(id1, id2)
    case (UnknownTypeResult(id), _) => addBound(id, b)
    case (_, UnknownTypeResult(id)) => addBound(id, a)
    case _ => this
  }

  def addEquivalence(a: TypeId, b: TypeId): CheckerContext = {
    new CheckerContext(equivalences + (a -> b) + (b -> a), bounds, stack, scopes)
  }

  def addBound(id: TypeId, bound: TypeResult): CheckerContext = bound match {
    case UnknownTypeResult(otherId) => addEquivalence(id, otherId)
    case _ if bounds.contains(id) =>
      val oldBound = bounds(id)
      val (newBound, newEquivalences) = (oldBound, bound) match {
        case (UnknownTypeResult(a), UnknownTypeResult(b)) => (oldBound, equivalences + (a -> b) + (b -> a))
        case (UnknownTypeResult(_), _) => (bound, equivalences)
        case (_, UnknownTypeResult(_)) => (oldBound, equivalences)
        case (a, b) if a == b => (oldBound, equivalences)
        case _ => (InvalidTypeResult(s"type mismatch: $oldBound vs $bound"), equivalences)
      }
      val newBounds = bounds + (id -> newBound)
      new CheckerContext(newEquivalences, newBounds, stack, scopes)
    case _ =>
      val newBounds = bounds + (id -> bound)
      new CheckerContext(equivalences, newBounds, stack, scopes)
  }

  private[this] def allEquivalences(id: TypeId, visited: Set[TypeId] = Set()): Set[TypeId] = {
    if (!visited.contains(id)) {
      equivalences.get(id) match {
        case Some(otherId) => allEquivalences(otherId, visited + id)
        case None => visited + id
      }
    } else {
      visited
    }
  }

  private[this] def allTypes(id: TypeId): Set[TypeResult] = {
    allEquivalences(id).flatMap(bounds.get)
  }

  def solve(id: TypeId): TypeResult = {
    allTypes(id).foldLeft(UnknownTypeResult(new TypeId): TypeResult) {
      case (UnknownTypeResult(_), b) => b
      case (a, UnknownTypeResult(_)) => a
      case (a, b) if a == b => a
      case (a, b) => InvalidTypeResult(s"type mismatch: $a vs $b")
    }
  }

  def solve(result: TypeResult): TypeResult = result match {
    case UnknownTypeResult(id) => solve(id)
    case _ => result
  }

}
