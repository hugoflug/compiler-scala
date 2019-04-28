import java.nio.ByteBuffer

import ConstantPoolUtil.{ClassRef, ConstantPoolEntry, ConstantPoolRef}


object Assembler {
  case class ClassFile(filename: String, content: Array[Byte])

  def assemble(clazz: ClassAssembly): ClassFile = {
    val cp = ConstantPoolUtil.constantPoolEntries(clazz)
    val cpMap = cp.map(e => (e.ref, e)).toMap
    val content = magicNumber ++
    minorVersion ++
    majorVersion ++
    constantPool(cp) ++
    classInfo(clazz.className, cpMap) ++
    classInfo(clazz.superClass, cpMap) ++
    interfaceTable ++
    fieldTable(clazz.fields, cp) ++
    methodTable(clazz.methods, cp) ++

    attributesTable

    ClassFile(clazz.className + ".class", content)
  }


  def hex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def fromUShort(i: Int): Array[Byte] = ByteBuffer.allocate(4).putInt(i).array()

  def fromShort(s: Short): Array[Byte] = ByteBuffer.allocate(2).putShort(s).array()

  def constantPool(entries: Set[ConstantPoolEntry]): Array[Byte] = ???

  def accessFlags = hex("0001")

  def classInfo(className: String, cp: Map[ConstantPoolRef, ConstantPoolEntry]): Array[Byte] =
    fromUShort(cp(ClassRef(className)).index)

  def interfaceTable: Array[Byte] = interfaceTableLength
  def interfaceTableLength = hex("0000")

  def fieldTable(fields: Seq[FieldAssembly], cp: Set[ConstantPoolEntry]): Array[Byte] = {
    fromShort(fields.length.toShort)

  }

  def methodTable(methods: Seq[MethodAssembly], cp: Set[ConstantPoolEntry]): Array[Byte] = ???

  def attributesTable: Array[Byte] = attributesTableLength
  def attributesTableLength = hex("0000")

  def magicNumber = hex("CAFEBABE")
  def minorVersion = hex("0003")
  def majorVersion = hex("002D")
}
