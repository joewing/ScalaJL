package net.joewing.jl

trait LambdaResult {
  val scope: ScopeId
}

object LambdaResult {
  def unapply(lambda: LambdaResult): Option[ScopeId] = Some(lambda.scope)
}
