package net.joewing.jl

abstract class Context[T, C <: Context[T, C]](
    val stack: List[ScopeId],
    protected val scopes: Map[ScopeId, Scope[T]]) {

  protected def create(stack: List[ScopeId], scopes: Map[ScopeId, Scope[T]]): C

  private[this] def lookupStack(name: String, lst: List[ScopeId]): Option[T] = lst match {
    case id :: tl => scopes(id).values.get(name).orElse(lookupStack(name, tl))
    case _ => None
  }

  def lookup(name: String): Option[T] = lookupStack(name, stack)

  def updateScope(newValues: Map[String, T]): C = {
    val scope = scopes(stack.head).update(newValues)
    val updatedScopes = scopes.updated(stack.head, scope)
    create(stack, updatedScopes)
  }

  def valueList(names: List[String]): List[T] = names.map { lookup(_).get }

  def enterScope: C = {
    val newScopeId = new ScopeId()
    val scope = new Scope[T](newScopeId, Map())
    create(newScopeId +: stack, scopes + (newScopeId -> scope))
  }

  def leaveScope: C = create(stack.tail, scopes)

}
