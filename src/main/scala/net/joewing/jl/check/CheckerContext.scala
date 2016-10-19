package net.joewing.jl.check

import net.joewing.jl.{Context, ScopeId, Token}

class CheckerContext(
    private val equivalences: Map[UnknownTypeResult, UnknownTypeResult],
    private val bounds: Map[UnknownTypeResult, TypeResult],
    private val generics: Map[UnknownTypeResult, GenericTypeResult],
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, CheckerScope])
  extends Context[TypeResult, CheckerScope, CheckerContext](_stack, _scopes) {

  protected def create(stack: List[ScopeId], scopes: Map[ScopeId, CheckerScope]): CheckerContext = {
    new CheckerContext(equivalences, bounds, generics, stack, scopes)
  }

  def newScope: CheckerScope = new CheckerScope(Map(), new ScopeId, Map())

  private[this] def resolveGeneric(token: Token, id: TypeId, lst: List[ScopeId]): TypeResult = lst match {
    case scopeId :: tl => scopes(scopeId).generics.getOrElse(id, resolveGeneric(token, id, tl))
    case _ => NilTypeResult(token)
  }

  def resolveGeneric(token: Token, id: TypeId): TypeResult = resolveGeneric(token, id, stack)

  def registerGeneric(token: Token, unknown: UnknownTypeResult): (CheckerContext, GenericTypeResult) = {
    generics.get(unknown) match {
      case Some(generic) => (this, generic)
      case None =>
        val newGeneric = GenericTypeResult(token, new TypeId)
        (new CheckerContext(equivalences, bounds, generics + (unknown -> newGeneric), stack, scopes), newGeneric)
    }
  }

  private[this] def addLambdaEquivalence(a: LambdaTypeResult, b: LambdaTypeResult): CheckerContext = {
    if (a.args.length != b.args.length) {
      addInvalid(b.token, "wrong number of arguments")
    } else {
      (a.args :+ a.ret).zip(b.args :+ b.ret).foldLeft(this) { (context, types) =>
        val (a, b) = types
        context.addEquivalence(a, b)
      }
    }
  }

  private[this] def addUnknownEquivalence(a: UnknownTypeResult, b: UnknownTypeResult): CheckerContext = {
    new CheckerContext(equivalences + (a -> b) + (b -> a), bounds, generics, stack, scopes)
  }

  private[this] def addGenericEquivalence(any: GenericTypeResult, other: TypeResult): CheckerContext = {
    val updatedOther = solve(other)
    val previous = scopes(stack.head).generics.get(any.id)
    val updatedScope = scopes(stack.head).setGeneric(any.id, updatedOther)
    val newScopes = scopes + (updatedScope.id -> updatedScope)
    val newContext = new CheckerContext(equivalences, bounds, generics, stack, newScopes)
    previous match {
      case Some(result) => newContext.addEquivalence(result, other)
      case None => newContext
    }
  }

  private[this] def addInvalid(token: Token, msg: String): CheckerContext = {
    val unknown = UnknownTypeResult(token, new TypeId)
    val invalid = InvalidTypeResult(token, msg)
    addBound(unknown, invalid)
  }

  private[this] def addBound(unknown: UnknownTypeResult, bound: TypeResult): CheckerContext = bound match {
    case other: UnknownTypeResult => addUnknownEquivalence(unknown, other)
    case _ if bounds.contains(unknown) => addEquivalence(bounds(unknown), bound)
    case _ => new CheckerContext(equivalences, bounds + (unknown -> bound), generics, stack, scopes)
  }

  def addEquivalence(a: TypeResult, b: TypeResult): CheckerContext = (a, b) match {
    case (t1 @ UnknownTypeResult(_, _), t2 @ UnknownTypeResult(_, _)) => addUnknownEquivalence(t1, t2)
    case (t1 @ UnknownTypeResult(_, _), _) => addBound(t1, b)
    case (_, t2 @ UnknownTypeResult(_, _)) => addBound(t2, a)
    case (t1 @ LambdaTypeResult(_, _, _), t2 @ LambdaTypeResult(_, _, _)) => addLambdaEquivalence(t1, t2)
    case (ListTypeResult(_, t1), ListTypeResult(_, t2)) => addEquivalence(t1, t2)
    case (t1 @ GenericTypeResult(_, _), t2) => addGenericEquivalence(t1, t2)
    case (t1, t2 @ GenericTypeResult(_, _)) => addGenericEquivalence(t2, t1)
    case (t1, t2) if t1 != t2 => addInvalid(t2.token, s"type mismatch: $t1 vs $t2")
    case _ => this
  }

  private[this] def allEquivalences(
      unknown: UnknownTypeResult,
      visited: Set[UnknownTypeResult] = Set()): Set[UnknownTypeResult] = {
    if (!visited.contains(unknown)) {
      equivalences.get(unknown) match {
        case Some(otherId) => allEquivalences(otherId, visited + unknown)
        case None => visited + unknown
      }
    } else {
      visited
    }
  }

  private[this] def allTypes(unknown: UnknownTypeResult): Set[TypeResult] = {
    allEquivalences(unknown).flatMap(bounds.get)
  }

  private[this] def getCanonicalUnknown(unknown: UnknownTypeResult): UnknownTypeResult = {
    allEquivalences(unknown).foldLeft(unknown) { (best, current) =>
      if (current.id < best.id) current else best
    }
  }

  def getUnknown(unknown: UnknownTypeResult): TypeResult = {
    val binding = allTypes(unknown).foldLeft(unknown: TypeResult) {
      case (a @ UnknownTypeResult(_,_), b @ UnknownTypeResult(_,_)) => if (a.id < b.id) a else b
      case (UnknownTypeResult(_, _), b) => b
      case (a, UnknownTypeResult(_, _)) => a
      case (a, b) if a == b => a
      case (a, b) => InvalidTypeResult(unknown.token, s"type mismatch: $a vs $b")
    }
    binding match {
      case _: UnknownTypeResult => getCanonicalUnknown(unknown)
      case _ => binding.solve(this)
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
