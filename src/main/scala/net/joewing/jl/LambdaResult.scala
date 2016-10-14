package net.joewing.jl

trait LambdaResult {
  val scope: Int
}

object LambdaResult {
  def unapply(lambda: LambdaResult): Option[Int] = Some(lambda.scope)
}
