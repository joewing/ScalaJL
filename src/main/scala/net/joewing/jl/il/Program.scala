package net.joewing.jl.il

case class Program(
    entry: InstructionId,
    instructions: List[Instruction],
    order: Map[InstructionId, InstructionId],
    values: Map[ValueId, ValueType]) {

  def newValue(t: ValueType): (Program, ValueId) = {
    val result = new ValueId
    val newValues = values + (result -> t)
    Program(entry, instructions, order, newValues)
  }

  def append(instruction: Instruction): Program = Program(entry, instructions :+ instruction, order, values)

  @annotation.tailrec
  private[this] def getLabeled(remaining: List[Instruction], labeled: Set[InstructionId] = Set()): Set[InstructionId] = {
    remaining match {
      case hd :: tl => getLabeled(tl, labeled ++ hd.links)
      case _ => Set(entry)
    }
  }

  @annotation.tailrec
  private[this] def getOutput(remaining: List[Instruction], labeled: Set[InstructionId], str: String = ""): String = {
    remaining match {
      case hd :: tl =>
        val current = if (labeled.contains(hd.id)) s"L${hd.id}:\t$hd\n" else s"\t$hd\n"
        getOutput(tl, labeled, str ++ current)
      case _ => ""
    }
  }

  override def toString = getOutput(instructions, getLabeled(instructions))
}
