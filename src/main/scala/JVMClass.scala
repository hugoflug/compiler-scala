case class JVMClass(className: String, superClass: String, fields: Seq[JVMField], methods: Seq[JVMMethod])
case class JVMMethod(name: String, typeDesc: String, static: Boolean, maxStack: Int, maxLocals: Int,
                     code: Seq[JVMInstruction])
case class JVMField(name: String, typeDesc: String)

sealed trait JVMInstruction

sealed trait InstructionWithLabel extends JVMInstruction {
  def label: Int
}

sealed trait InstructionWithVarNo extends JVMInstruction {
  def varNo: Int
}

sealed trait InstructionWithFieldRef extends JVMInstruction {
  def clazz: String
  def name: String
  def typeDesc: String
}

sealed trait InstructionWithMethodRef extends JVMInstruction {
  def clazz: String
  def name: String
  def typeDesc: String
}

case class Iconst_0() extends JVMInstruction
case class Iconst_1() extends JVMInstruction
case class Goto(label: Int) extends InstructionWithLabel
case class Dup() extends JVMInstruction
case class Pop() extends JVMInstruction
case class Label(id: Int) extends JVMInstruction
case class Istore(varNo: Int) extends InstructionWithVarNo
case class Astore(varNo: Int) extends InstructionWithVarNo
case class Aload_0() extends JVMInstruction
case class Swap() extends JVMInstruction
case class Putfield(clazz: String, name: String, typeDesc: String) extends InstructionWithFieldRef
case class Return() extends JVMInstruction
case class Ireturn() extends JVMInstruction
case class Areturn() extends JVMInstruction
case class Ifeq(label: Int) extends InstructionWithLabel
case class Getstatic(clazz: String, name: String, typeDesc: String) extends InstructionWithFieldRef
case class Invokevirtual(clazz: String, name: String, typeDesc: String, args: Int) extends InstructionWithMethodRef
case class Ldc_wInt(value: Int) extends JVMInstruction
case class Ldc_wString(s: String) extends JVMInstruction
case class Iadd() extends JVMInstruction
case class Isub() extends JVMInstruction
case class If_icmpgt(label: Int) extends InstructionWithLabel
case class If_icmpge(label: Int) extends InstructionWithLabel
case class If_icmplt(label: Int) extends InstructionWithLabel
case class If_icmple(label: Int) extends InstructionWithLabel
case class Imul() extends JVMInstruction
case class Ixor() extends JVMInstruction
case class New_array(type_ : Int) extends JVMInstruction
case class Invokespecial(clazz: String, name: String, typeDesc: String) extends InstructionWithMethodRef
case class Array_length() extends JVMInstruction
case class Iaload() extends JVMInstruction
case class Ifne(label: Int) extends InstructionWithLabel
case class If_acmpeq(label: Int) extends InstructionWithLabel
case class If_icmpeq(label: Int) extends InstructionWithLabel
case class If_acmpne(label: Int) extends InstructionWithLabel
case class If_icmpne(label: Int) extends InstructionWithLabel
case class Iload(varNo: Int) extends InstructionWithVarNo
case class Aload(varNo: Int) extends InstructionWithVarNo
case class Iastore() extends JVMInstruction
case class Getfield(clazz: String, name: String, typeDesc: String) extends InstructionWithFieldRef
case class New(clazz: String) extends JVMInstruction
case class Nop() extends JVMInstruction