import ConstantPoolUtil.ConstantPoolRef

object InstructionTable {
  def mkBytes(instruction: AssemblyInstruction, cpIndex: Map[ConstantPoolRef, Int]): Array[Byte] = instruction match {
    case labelInst: InstructionWithLabel => labelInst match {
      case Goto(label) =>
      case Ifeq(label) =>
      case If_icmpgt(label) =>
      case If_icmpge(label) =>
      case If_icmplt(label) =>
      case If_icmple(label) =>
      case Ifne(label) =>
      case If_acmpeq(label) =>
      case If_icmpeq(label) =>
      case If_acmpne(label) =>
      case If_icmpne(label) =>
    }

    case varNoInst: InstructionWithVarNo => varNoInst match {
      case Istore(varNo) =>
      case Astore(varNo) =>
      case Iload(varNo) =>
      case Aload(varNo) =>
    }

    case Iconst_0() =>
    case Iconst_1() =>
    case Dup() =>
    case Pop() =>
    case Label(id) =>
    case Aload_0() =>
    case Swap() =>
    case Putfield(clazz, name, typeDesc) =>
    case Return() =>
    case Ireturn() =>
    case Areturn() =>
    case Getstatic(clazz, name, typeDesc) =>
    case Invokevirtual(clazz, name, typeDesc) =>
    case LdcInt(value) =>
    case LdcString(s) =>
    case Iadd() =>
    case Isub() =>
    case Imul() =>
    case Ixor() =>
    case Newarray(type_) =>
    case Invokespecial(clazz, name, typeDesc) =>
    case Arraylength() =>
    case Iaload() =>
    case Iastore() =>
    case Getfield(clazz, name, typeDesc) =>
    case New(clazz) =>
  }
}
