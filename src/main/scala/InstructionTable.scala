import ConstantPoolUtil.{ClassRef, ConstantPoolRef, FieldRef, IntRef, MethodRef, StringRef}
import ByteUtils._

object InstructionTable {

  def mkBytes(instructions: Seq[AssemblyInstruction], cpIndex: Map[ConstantPoolRef, Int]) = {
    val labelByteOffsets = labelOffsets(instructions)

    val (bytes, _) = instructions.foldLeft((Array[Byte](), 0)) {
      case ((cBytes, offset), instruction) =>
        val instrBytes = mkBytes(instruction, cpIndex, offset, labelByteOffsets)
        (cBytes ++ instrBytes, offset + instructionSize(instruction))
    }

    bytes
  }

  private def mkBytes(instruction: AssemblyInstruction, cpIndex: Map[ConstantPoolRef, Int], offset: Int,
                      labelOffsets: Map[Int, Int]): Array[Byte] =
    instructionCode(instruction) ++ extraBytes(instruction, cpIndex, offset, labelOffsets)

  private def extraBytes(instruction: AssemblyInstruction, cpIndex: Map[ConstantPoolRef, Int], offset: Int,
                         labelOffsets: Map[Int, Int]) =
    instruction match {
      case i: InstructionWithLabel =>
        (labelOffsets(i.label) - offset).s2

      case i: InstructionWithVarNo =>
        i.varNo.u1

      case i: InstructionWithFieldRef =>
        cpIndex(FieldRef(i.clazz, i.name, i.typeDesc)).u2

      case i: InstructionWithMethodRef =>
        cpIndex(MethodRef(i.clazz, i.name, i.typeDesc)).u2

      case LdcInt(value) =>
        cpIndex(IntRef(value)).u2

      case LdcString(s) =>
        cpIndex(StringRef(s)).u2

      case Newarray(type_) =>
        type_.u1

      case New(clazz) =>
        cpIndex(ClassRef(clazz)).u2

      case _ => "".bytes
    }

  private def instructionCode(instruction: AssemblyInstruction): Array[Byte] = instruction match {
    case _: Goto => "a7".bytes
    case _: Ifeq => "99".bytes
    case _: If_icmpgt => "a3".bytes
    case _: If_icmpge => "a2".bytes
    case _: If_icmplt => "a1".bytes
    case _: If_icmple => "a4".bytes
    case _: Ifne => "9a".bytes
    case _: If_acmpeq => "a5".bytes
    case _: If_icmpeq => "9f".bytes
    case _: If_acmpne => "a6".bytes
    case _: If_icmpne => "a0".bytes
    case _: Istore => "36".bytes
    case _: Astore => "3a".bytes
    case _: Iload => "15".bytes
    case _: Aload => "19".bytes
    case _: Iconst_0 => "03".bytes
    case _: Iconst_1 => "04".bytes
    case _: Dup => "59".bytes
    case _: Pop => "57".bytes
    case _: Label => "".bytes
    case _: Aload_0 => "2a".bytes
    case _: Swap => "5f".bytes
    case _: Putfield => "b5".bytes
    case _: Return => "b1".bytes
    case _: Ireturn => "ac".bytes
    case _: Areturn => "b0".bytes
    case _: Getstatic => "b2".bytes
    case _: Invokevirtual => "b6".bytes
    case _: LdcInt => "12".bytes
    case _: LdcString => "12".bytes
    case _: Iadd => "60".bytes
    case _: Isub => "64".bytes
    case _: Imul => "68".bytes
    case _: Ixor => "82".bytes
    case _: Newarray => "bc".bytes
    case _: Invokespecial => "b7".bytes
    case _: Arraylength => "be".bytes
    case _: Iaload => "2e".bytes
    case _: Iastore => "4f".bytes
    case _: Getfield => "b4".bytes
    case _: New => "bb".bytes
  }

  private def labelOffsets(instructions: Seq[AssemblyInstruction]): Map[Int, Int] = {
    val (labelOffsetMap, _) = instructions.foldLeft((Map[Int, Int](), 0)) {
      case ((offsetMap, offset), instruction) =>
        instruction match {
          case Label(id) => (offsetMap + (id -> offset), offset)
          case instr: AssemblyInstruction => (offsetMap, offset + instructionSize(instr))
        }
    }

    labelOffsetMap
  }

  private def instructionSize(instruction: AssemblyInstruction) = instruction match {
      case _: InstructionWithLabel => 3
      case _: InstructionWithVarNo => 2
      case _: Iconst_0 => 1
      case _: Iconst_1 => 1
      case _: Dup => 1
      case _: Pop => 1
      case _: Label => 0
      case _: Aload_0 => 0
      case _: Swap => 0
      case _: Putfield => 3
      case _: Return => 1
      case _: Ireturn => 1
      case _: Areturn => 1
      case _: Getstatic => 3
      case _: Invokevirtual => 3
      case _: LdcInt => 2
      case _: LdcString => 2
      case _: Iadd => 1
      case _: Isub => 1
      case _: Imul => 1
      case _: Ixor => 1
      case _: Newarray => 2
      case _: Invokespecial => 3
      case _: Arraylength => 1
      case _: Iaload => 1
      case _: Iastore => 1
      case _: Getfield => 3
      case _: New => 3
    }
}
