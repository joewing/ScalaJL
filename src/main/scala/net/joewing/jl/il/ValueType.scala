package net.joewing.jl.il

sealed trait ValueType

case class IntegerValueType() extends ValueType
case class StringValueType() extends ValueType
case class LambdaValueType() extends ValueType
