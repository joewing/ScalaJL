package net.joewing.jl

abstract class Runner[T] {

  protected val nil: T

  def run(context: Context[T], token: Token): (Context[T], T)

  final def run(context: Context[T], tokens: List[Token]): (Context[T], T) = {
    tokens.foldLeft((context, nil)) { (acc, token) =>
      val (oldContext, _) = acc
      run(oldContext, token)
    }
  }
}
