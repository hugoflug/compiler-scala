import SymbolTableCreator.SymbolTable

object StackDepthCalculator {
  def maxStackDepth(instructions: Seq[JVMInstruction], symTable: SymbolTable) = {
    val (_, maxDepth) = instructions.foldLeft((0, 0)) {
      case ((current, max), instruction) =>
        val newCurrent = current + stackChange(instruction, symTable)
        (newCurrent, max.max(newCurrent))
    }

    maxDepth
  }

  private def stackChange(instruction: JVMInstruction, symTable: SymbolTable): Int =
    instruction match {
      case Goto(_) => 0
      case Ifeq(_) => -1
      case If_icmpgt(_) => -2
      case If_icmpge(_) => -2
      case If_icmplt(_) => -2
      case If_icmple(_) => -2
      case Ifne(_) => -1
      case If_acmpeq(_) => -2
      case If_icmpeq(_) => -2
      case If_acmpne(_) => -2
      case If_icmpne(_) => -2
      case Istore(_) => -1
      case Astore(_) => -1
      case Iload(_) => +1
      case Aload(_) => +1
      case Putfield(_, _, _) => -2
      case Getstatic(_, _, _) => +1
      case Getfield(_, _, _) => 0
      case Invokevirtual(_, _, _, argCount) => -argCount
      case Invokespecial(_, _, _) => -1
      case Iconst_0() => +1
      case Iconst_1() => +1
      case Dup() => +1
      case Pop() => -1
      case Label(_) => 0
      case Aload_0() => +1
      case Swap() => 0
      case Return() => 0
      case Ireturn() => -1
      case Areturn() => -1
      case Ldc_wInt(_) => +1
      case Ldc_wString(_) => +1
      case Iadd() => -1
      case Isub() => -1
      case Imul() => -1
      case Ixor() => -1
      case New_array(_) => 0
      case Array_length() => 0
      case Iaload() => -1
      case Iastore() => -3
      case New(_) => +1
      case Nop() => 0
  }
}
