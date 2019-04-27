case class ClassAssembly(className: String, superClass: String, fields: Seq[FieldAssembly], methods: Seq[MethodAssembly])
case class MethodAssembly(name: String, typeDesc: String, maxStack: Int, maxLocals: Int, code: Seq[AssemblyInstruction])
case class FieldAssembly(name: String, typeDesc: String)

sealed trait AssemblyInstruction

sealed trait InstructionWithLabel extends AssemblyInstruction {
  def label: Int
}

sealed trait InstructionWithVarNo extends AssemblyInstruction {
  def varNo: Int
}

case class Iconst_0() extends AssemblyInstruction
case class Iconst_1() extends AssemblyInstruction
case class Goto(label: Int) extends InstructionWithLabel
case class Dup() extends AssemblyInstruction
case class Pop() extends AssemblyInstruction
case class Label(id: Int) extends AssemblyInstruction
case class Istore(varNo: Int) extends InstructionWithVarNo
case class Astore(varNo: Int) extends InstructionWithVarNo
case class Aload_0() extends AssemblyInstruction
case class Swap() extends AssemblyInstruction
case class Putfield(clazz: String, name: String, typeDesc: String) extends AssemblyInstruction
case class Return() extends AssemblyInstruction
case class Ireturn() extends AssemblyInstruction
case class Areturn() extends AssemblyInstruction
case class Ifeq(label: Int) extends InstructionWithLabel
case class Getstatic(clazz: String, name: String, typeDesc: String) extends AssemblyInstruction
case class Invokevirtual(clazz: String, name: String, typeDesc: String) extends AssemblyInstruction
case class LdcInt(value: Int) extends AssemblyInstruction
case class LdcString(s: String) extends AssemblyInstruction
case class Iadd() extends AssemblyInstruction
case class Isub() extends AssemblyInstruction
case class If_icmpgt(label: Int) extends InstructionWithLabel
case class If_icmpge(label: Int) extends InstructionWithLabel
case class If_icmplt(label: Int) extends InstructionWithLabel
case class If_icmple(label: Int) extends InstructionWithLabel
case class Imul() extends AssemblyInstruction
case class Ixor() extends AssemblyInstruction
case class Newarray(type_ : Int) extends AssemblyInstruction
case class Invokespecial(clazz: String, name: String, typeDesc: String) extends AssemblyInstruction
case class Arraylength() extends AssemblyInstruction
case class Iaload() extends AssemblyInstruction
case class Ifne(label: Int) extends InstructionWithLabel
case class If_acmpeq(label: Int) extends InstructionWithLabel
case class If_icmpeq(label: Int) extends InstructionWithLabel
case class If_acmpne(label: Int) extends InstructionWithLabel
case class If_icmpne(label: Int) extends InstructionWithLabel
case class Iload(varNo: Int) extends InstructionWithVarNo
case class Aload(varNo: Int) extends InstructionWithVarNo
case class Iastore() extends AssemblyInstruction
case class Getfield(clazz: String, name: String, typeDesc: String) extends AssemblyInstruction
case class New(clazz: String) extends AssemblyInstruction