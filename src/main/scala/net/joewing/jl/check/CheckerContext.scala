package net.joewing.jl.check

import net.joewing.jl.{Context, ScopeId, Token}

class CheckerContext(
    private val equivalences: Map[TypeId, TypeId],
    private val bounds: Map[TypeId, TypeResult],
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, CheckerScope])
  extends Context[TypeResult, CheckerScope, CheckerContext](_stack, _scopes) {

  protected def create(stack: List[ScopeId], scopes: Map[ScopeId, CheckerScope]): CheckerContext = {
    new CheckerContext(equivalences, bounds, stack, scopes)
  }

  def newScope: CheckerScope = new CheckerScope(Map(), new ScopeId(), Map())

  private[this] def addLambdaEquivalence(a: LambdaTypeResult, b: LambdaTypeResult): CheckerContext = {
    if (a.args.length != b.args.length) {
      addBound(new TypeId(), InvalidTypeResult(b.token, "wrong number of arguments"))
    } else {
      (a.args :+ a.ret).zip(b.args :+ b.ret).foldLeft(this) { (context, types) =>
        val (a, b) = types
        context.addEquivalence(a, b)
      }
    }
  }

  private[this] def addUnknownEquivalence(id1: TypeId, id2: TypeId): CheckerContext = {
    new CheckerContext(equivalences + (id1 -> id2) + (id2 -> id1), bounds, stack, scopes)
  }

  private[this] def addGenericEquivalence(any: AnyTypeResult, other: TypeResult): CheckerContext = {
    val updatedScope = scopes(stack.head).setGeneric(any.id, other)
    val newScopes = scopes + (updatedScope.id -> updatedScope)
    new CheckerContext(equivalences, bounds, stack, newScopes)
  }

  private[this] def addBound(id: TypeId, bound: TypeResult): CheckerContext = bound match {
    case UnknownTypeResult(_, otherId) => addUnknownEquivalence(id, otherId)
    case _ if bounds.contains(id) => addEquivalence(bounds(id), bound)
    case _ => new CheckerContext(equivalences, bounds + (id -> bound), stack, scopes)
  }

  def addEquivalence(a: TypeResult, b: TypeResult): CheckerContext = (a, b) match {
    case (UnknownTypeResult(_, id1), UnknownTypeResult(_, id2)) => addUnknownEquivalence(id1, id2)
    case (UnknownTypeResult(_, id), _) => addBound(id, b)
    case (_, UnknownTypeResult(_, id)) => addBound(id, a)
    case (t1 @ LambdaTypeResult(_, _, _), t2 @ LambdaTypeResult(_, _, _)) => addLambdaEquivalence(t1, t2)
    case (ListTypeResult(_, t1), ListTypeResult(_, t2)) => addEquivalence(t1, t2)
    case (t1 @ AnyTypeResult(_, _), t2) => addGenericEquivalence(t1, t2)
    case (t1, t2 @ AnyTypeResult(_, _)) => addGenericEquivalence(t2, t1)
    case (t1, t2) if t1 != t2 => addBound(new TypeId(), InvalidTypeResult(t2.token, s"type mismatch: $t1 vs $t2"))
    case _ => this
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

  def solve(token: Token, id: TypeId): TypeResult = {
    allTypes(id).foldLeft(UnknownTypeResult(token, new TypeId): TypeResult) {
      case (UnknownTypeResult(_, _), b) => b
      case (a, UnknownTypeResult(_, _)) => a
      case (a, b) if a == b => a
      case (a, b @ AnyTypeResult(_, _)) => println("one"); addEquivalence(a, b).solve(token, id)
      case (a @ AnyTypeResult(_, _), b) => println("two"); addEquivalence(b, a).solve(token, id)
      case (a, b) => println("what"); InvalidTypeResult(token, s"type mismatch: $a vs $b")
    }
  }

  def solve(result: TypeResult): TypeResult = {
    bounds.values.foldLeft(result.solve(this)) { (acc, value) =>
      (acc, value) match {
        case (left @ InvalidTypeResult(_, _), right @ InvalidTypeResult(_, _)) => InvalidTypeResult(left, right)
        case (_, invalid @ InvalidTypeResult(_, _)) => invalid
        case _ => acc
      }
    }
  }

}
