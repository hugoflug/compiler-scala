import ConstantPoolUtil.ConstantPoolEntry

object Assembler {
  case class ClassFile(filename: String, content: Array[Byte])

  def assemble(clazz: ClassAssembly): ClassFile = {
    val cp = ConstantPoolUtil.constantPoolEntries(clazz)
    val content = magicNumber ++
    minorVersion ++
    majorVersion ++
    constantPool(cp) ++
    interfaceTable ++
    fieldTable(clazz.fields, cp) ++
    methodTable(clazz.methods, cp) ++
    attributesTable

    ClassFile(clazz.className + ".class", content)
  }

  def hex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def constantPool(entries: Set[ConstantPoolEntry]): Array[Byte] = ???

  def interfaceTable: Array[Byte] = interfaceTableLength
  def interfaceTableLength = hex("0000")

  def attributesTable: Array[Byte] = attributesTableLength
  def attributesTableLength = hex("0000")

  def fieldTable(fields: Seq[FieldAssembly], cp: Set[ConstantPoolEntry]): Array[Byte] = ???

  def methodTable(methods: Seq[MethodAssembly], cp: Set[ConstantPoolEntry]): Array[Byte] = ???

  def magicNumber = hex("CAFEBABE")
  def minorVersion = hex("0003")
  def majorVersion = hex("002D")
}
