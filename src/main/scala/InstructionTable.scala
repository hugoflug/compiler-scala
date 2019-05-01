import ConstantPoolUtil.{ClassRef, ConstantPoolRef, FieldRef, IntRef, MethodRef, StringRef}
import ByteUtils._

object InstructionTable {

  def mkBytes(instructions: Seq[JVMInstruction], cpIndex: Map[ConstantPoolRef, Int]) = {
    val labelByteOffsets = labelOffsets(instructions)

    val (bytes, _) = instructions.foldLeft((Array[Byte](), 0)) {
      case ((cBytes, offset), instruction) =>
        val instrBytes = mkBytes(instruction, cpIndex, offset, labelByteOffsets)
        (cBytes ++ instrBytes, offset + instructionSize(instruction))
    }

    bytes
  }

  private def mkBytes(instruction: JVMInstruction, cpIndex: Map[ConstantPoolRef, Int], offset: Int,
                      labelOffsets: Map[Int, Int]): Array[Byte] =
    instructionCode(instruction) ++ extraBytes(instruction, cpIndex, offset, labelOffsets)

  private def extraBytes(instruction: JVMInstruction, cpIndex: Map[ConstantPoolRef, Int], offset: Int,
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

      case _ => "".hex
    }

  private def instructionCode(instruction: JVMInstruction): Array[Byte] = instruction match {
    case _: Goto => "a7".hex
    case _: Ifeq => "99".hex
    case _: If_icmpgt => "a3".hex
    case _: If_icmpge => "a2".hex
    case _: If_icmplt => "a1".hex
    case _: If_icmple => "a4".hex
    case _: Ifne => "9a".hex
    case _: If_acmpeq => "a5".hex
    case _: If_icmpeq => "9f".hex
    case _: If_acmpne => "a6".hex
    case _: If_icmpne => "a0".hex
    case _: Istore => "36".hex
    case _: Astore => "3a".hex
    case _: Iload => "15".hex
    case _: Aload => "19".hex
    case _: Iconst_0 => "03".hex
    case _: Iconst_1 => "04".hex
    case _: Dup => "59".hex
    case _: Pop => "57".hex
    case _: Label => "".hex
    case _: Aload_0 => "2a".hex
    case _: Swap => "5f".hex
    case _: Putfield => "b5".hex
    case _: Return => "b1".hex
    case _: Ireturn => "ac".hex
    case _: Areturn => "b0".hex
    case _: Getstatic => "b2".hex
    case _: Invokevirtual => "b6".hex
    case _: LdcInt => "12".hex
    case _: LdcString => "12".hex
    case _: Iadd => "60".hex
    case _: Isub => "64".hex
    case _: Imul => "68".hex
    case _: Ixor => "82".hex
    case _: Newarray => "bc".hex
    case _: Invokespecial => "b7".hex
    case _: Arraylength => "be".hex
    case _: Iaload => "2e".hex
    case _: Iastore => "4f".hex
    case _: Getfield => "b4".hex
    case _: New => "bb".hex
  }

  private def labelOffsets(instructions: Seq[JVMInstruction]): Map[Int, Int] = {
    val (labelOffsetMap, _) = instructions.foldLeft((Map[Int, Int](), 0)) {
      case ((offsetMap, offset), instruction) =>
        instruction match {
          case Label(id) => (offsetMap + (id -> offset), offset)
          case instr: JVMInstruction => (offsetMap, offset + instructionSize(instr))
        }
    }

    labelOffsetMap
  }

  private def instructionSize(instruction: JVMInstruction) = instruction match {
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
