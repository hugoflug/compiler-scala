import java.nio.ByteBuffer

import ConstantPoolUtil._
import ByteUtils._
//import ByteArrayConversions._
import fs2.{Chunk, Pure, Stream}

object Assembler {
  case class ClassFile(filename: String, content: Array[Byte])

  def assemble(clazz: ClassAssembly): ClassFile = {
    val cp = ConstantPoolUtil.constantPoolEntries(clazz)
    val cpIndex = cp.map(e => (e.ref, e.index)).toMap
    val cpEntryList = cp.toList.sortBy(_.index)

    /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 */
    val content = magicNumber ++
      minorVersion ++
      majorVersion ++
      constantPool(cpEntryList) ++
      classInfo(clazz.className, cpIndex) ++
      classInfo(clazz.superClass, cpIndex) ++
      accessFlags ++
      interfaceTable ++
      fieldTable(clazz.fields, cpIndex) ++
      methodTable(clazz.methods, cpIndex) ++
      attributesTable

    ClassFile(clazz.className + ".class", content)
  }
  
  private type CpIndex = Map[ConstantPoolRef, Int]

  private def magicNumber = "CAFEBABE".bytes
  private def minorVersion = 0.u2 ++ 3.u2
  private def majorVersion = 0.u2 ++ 45.u2

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4 */
  private def constantPool(entries: Seq[ConstantPoolEntry]): Array[Byte] = ???

  private def classInfo(className: String, cpIndex: CpIndex) =
    cpIndex(ClassRef(className)).u2

  private def accessFlags = publicAccess
  private def publicAccess = "0001".bytes

  private def interfaceTable = interfaceTableLength
  private def interfaceTableLength = 0.u2

  private def fieldTable(fields: Seq[FieldAssembly], cpIndex: CpIndex) =
    fields.length.u2 ++
    fields.flatMap(fieldInfo(_, cpIndex))

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5 */
  private def fieldInfo(field: FieldAssembly, cpIndex: CpIndex): Array[Byte] =
    publicAccess ++
    cpIndex(StringRef(field.name)).u2 ++
    cpIndex(StringRef(field.typeDesc)).u2 ++
    fieldAttributes

  private def fieldAttributes = fieldAttributesLength
  private def fieldAttributesLength = 0.u2

  private def methodTable(methods: Seq[MethodAssembly], cpIndex: CpIndex) =
    methods.length.u2 ++
    methods.flatMap(methodInfo(_, cpIndex))

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6 */
  private def methodInfo(method: MethodAssembly, cpIndex: CpIndex) =
    publicAccess ++
    cpIndex(StringRef(method.name)).u2 ++
    cpIndex(StringRef(method.typeDesc)).u2 ++
    methodAttributes(method, cpIndex)

  private def methodAttributesLength = 1.u2

  private def methodAttributes(method: MethodAssembly, cpIndex: CpIndex) =
    methodAttributesLength ++
    methodCodeAttribute(method, cpIndex)

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.3 */
  private def methodCodeAttribute(method: MethodAssembly, cpIndex: CpIndex) = {
    val body = methodCodeAttributeBody(method, cpIndex)

    cpIndex(StringRef("Code")).u2 ++
    body.length.u4 ++
    body
  }

  private def methodCodeAttributeBody(method: MethodAssembly, cpIndex: CpIndex) = {
    val code = methodCode(method, cpIndex)

    method.maxStack.u2 ++
    method.maxLocals.u2 ++
    code.length.u4 ++
    code ++
    exceptionTable ++
    methodAttributesTable
  }

  private def methodCode(method: MethodAssembly, cpIndex: CpIndex) =
    InstructionTable.mkBytes(method.code, cpIndex)

  private def exceptionTable = exceptionTableLength
  private def exceptionTableLength = 0.u2

  private def methodAttributesTable = methodAttributesTableLength
  private def methodAttributesTableLength = 0.u2


  def attributesTable = attributesTableLength
  def attributesTableLength = 0.u2
}
