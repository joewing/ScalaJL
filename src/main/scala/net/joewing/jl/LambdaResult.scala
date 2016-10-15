package net.joewing.jl

trait LambdaResult {
  val stack: List[ScopeId]
}

object LambdaResult {
  def unapply(lambda: LambdaResult): Option[List[ScopeId]] = Some(lambda.stack)
}
