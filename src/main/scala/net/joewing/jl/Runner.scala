package net.joewing.jl

abstract class Runner[T, C <: Context[T, C]] {

  protected val nil: T

  def run(context: C, token: Token): (C, T)

  final def run(context: C, tokens: List[Token]): (C, T) = {
    tokens.foldLeft((context, nil)) { (acc, token) =>
      val (oldContext, _) = acc
      run(oldContext, token)
    }
  }
}
