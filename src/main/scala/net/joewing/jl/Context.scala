package net.joewing.jl

abstract class Context[T, SCOPE <: Scope[T, SCOPE], CONTEXT <: Context[T, SCOPE, CONTEXT]](
    val stack: List[ScopeId],
    protected val scopes: Map[ScopeId, SCOPE]) {
  self: CONTEXT =>

  protected def create(stack: List[ScopeId], scopes: Map[ScopeId, SCOPE]): CONTEXT

  def newScope: SCOPE

  private[this] def lookupStack(name: String, lst: List[ScopeId]): Option[T] = lst match {
    case id :: tl => scopes(id).values.get(name).orElse(lookupStack(name, tl))
    case _ => None
  }

  def lookup(name: String): Option[T] = lookupStack(name, stack)

  def updateScope(newValues: Map[String, T]): CONTEXT = {
    val scope = scopes(stack.head).update(newValues)
    val updatedScopes = scopes.updated(stack.head, scope)
    create(stack, updatedScopes)
  }

  def valueList(names: List[String]): List[T] = names.map { lookup(_).get }

  def enterScope: CONTEXT = {
    val scope = newScope
    create(scope.id +: stack, scopes + (scope.id -> scope))
  }

  def leaveScope: CONTEXT = create(stack.tail, scopes)

  def map[A, B](lst: List[A])(f: (CONTEXT, A) => (CONTEXT, B)): (CONTEXT, List[B]) = {
    lst.foldLeft((this, List[B]())) { (acc, value) =>
      val (oldContext, oldValues) = acc
      val (newContext, newValue) = f(oldContext, value)
      (newContext, oldValues :+ newValue)
    }
  }

  def fold[A, B](lst: List[A])(zero: B)(f: (CONTEXT, B, A) => (CONTEXT, B)) = {
    lst.foldLeft((this, zero)) { (acc, value) =>
      val (oldContext, oldValue) = acc
      f(oldContext, oldValue, value)
    }
  }

}
