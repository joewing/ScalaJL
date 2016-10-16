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

  private[this] def addLambdaEquivalence(a: LambdaTypeResult, b: LambdaTypeResult): CheckerContext = {
    if (a.args.length != b.args.length) {
      addBound(new TypeId(), InvalidTypeResult(s"wrong number of arguments"))
    } else {
      (a.args :+ a.ret).zip(b.args :+ b.ret).foldLeft(this) { (context, types) =>
        val (a, b) = types
        context.addEquivalence(a, b)
      }
    }
  }

  def addEquivalence(a: TypeResult, b: TypeResult): CheckerContext = (a, b) match {
    case (UnknownTypeResult(id1), UnknownTypeResult(id2)) => addEquivalence(id1, id2)
    case (UnknownTypeResult(id), _) => addBound(id, b)
    case (_, UnknownTypeResult(id)) => addBound(id, a)
    case (t1 @ LambdaTypeResult(_, _), t2 @ LambdaTypeResult(_, _)) => addLambdaEquivalence(t1, t2)
    case (t1, t2) if t1 != t2 => addBound(new TypeId(), InvalidTypeResult(s"type mismatch: $t1 vs $t2"))
    case _ => this
  }

  def addEquivalence(a: TypeId, b: TypeId): CheckerContext = {
    new CheckerContext(equivalences + (a -> b) + (b -> a), bounds, stack, scopes)
  }

  def addBound(id: TypeId, bound: TypeResult): CheckerContext = bound match {
    case UnknownTypeResult(otherId) => addEquivalence(id, otherId)
    case _ if bounds.contains(id) => addEquivalence(bounds(id), bound)
    case _ => new CheckerContext(equivalences, bounds + (id -> bound), stack, scopes)
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

  def solve(result: TypeResult): TypeResult = {
    bounds.values.foldLeft(result.solve(this)) { (acc, value) =>
      (acc, value) match {
        case (_, InvalidTypeResult(_)) => value
        case _ => acc
      }
    }
  }

}
