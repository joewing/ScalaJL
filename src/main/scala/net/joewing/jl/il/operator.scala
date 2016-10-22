package net.joewing.jl.il

sealed trait Operator

case class Add() extends Operator
case class Subtract() extends Operator
case class Multiply() extends Operator
case class Divide() extends Operator
case class EQ() extends Operator
case class NE() extends Operator
case class LT() extends Operator
case class LE() extends Operator
case class GT() extends Operator
case class GE() extends Operator

