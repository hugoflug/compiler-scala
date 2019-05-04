import java.nio.charset.StandardCharsets

import ConstantPoolUtil._
import ByteUtils._
import FileUtils.FileOutput

object Assembler {
  def assemble(clazz: JVMClass): FileOutput = {
    val cp = ConstantPoolUtil.constantPoolEntries(clazz)
    val cpIndex = cp.map(e => (e.ref, e.index)).toMap
    val cpEntryList = cp.toList.sortBy(_.index)

    /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1 */
    val content = magicNumber ++
      minorVersion ++
      majorVersion ++
      constantPool(cpEntryList) ++
      accessFlags ++
      classInfo(clazz.className, cpIndex) ++
      classInfo(clazz.superClass, cpIndex) ++
      interfaceTable ++
      fieldTable(clazz.fields, cpIndex) ++
      methodTable(clazz.methods, cpIndex) ++
      attributesTable

    FileOutput(clazz.className + ".class", content)
  }
  
  private type ConstantPoolIndex = Map[ConstantPoolRef, Int]

  private def magicNumber = "CAFEBABE".hex
  private def minorVersion = 3.u2
  private def majorVersion = 45.u2

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4 */
  private def constantPool(entries: Seq[ConstantPoolEntry]) =
    (entries.length + 1).u2 ++
    entries.toArray.flatMap(constantPoolEntry)

  private def constantPoolEntry(entry: ConstantPoolEntry) =
    entry match {
      case Utf8Entry(Utf8Ref(s), _) =>
        val bytes = s.getBytes(StandardCharsets.UTF_8)
        1.u1 ++ bytes.length.u2 ++ bytes
      case StringEntry(_, _, utf8Index) => 8.u1 ++ utf8Index.u2
      case IntEntry(IntRef(i), _) => 3.u1 ++ i.s4
      case ClassEntry(_, _, nameIndex) => 7.u1 ++ nameIndex.u2
      case FieldEntry(_, _, classIndex, natIndex) => 9.u1 ++ classIndex.u2 ++ natIndex.u2
      case MethodEntry(_, _, classIndex, natIndex) => 10.u1 ++ classIndex.u2 ++ natIndex.u2
      case NatEntry(_, _, nameIndex, typeIndex) => 12.u1 ++ nameIndex.u2 ++ typeIndex.u2
    }

  private def classInfo(className: String, cpIndex: ConstantPoolIndex) =
    cpIndex(ClassRef(className)).u2

  private def accessFlags = publicAccess
  private def publicAccess = "0001".hex
  private def publicStaticAccess = "0009".hex

  private def interfaceTable = interfaceTableLength
  private def interfaceTableLength = 0.u2

  private def fieldTable(fields: Seq[JVMField], cpIndex: ConstantPoolIndex) =
    fields.length.u2 ++
    fields.flatMap(fieldInfo(_, cpIndex))

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5 */
  private def fieldInfo(field: JVMField, cpIndex: ConstantPoolIndex) =
    publicAccess ++
    cpIndex(Utf8Ref(field.name)).u2 ++
    cpIndex(Utf8Ref(field.typeDesc)).u2 ++
    fieldAttributes

  private def fieldAttributes = fieldAttributesLength
  private def fieldAttributesLength = 0.u2

  private def methodTable(methods: Seq[JVMMethod], cpIndex: ConstantPoolIndex) =
    methods.length.u2 ++
    methods.flatMap(methodInfo(_, cpIndex))

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6 */
  private def methodInfo(method: JVMMethod, cpIndex: ConstantPoolIndex) =
    methodAccess(method) ++
    cpIndex(Utf8Ref(method.name)).u2 ++
    cpIndex(Utf8Ref(method.typeDesc)).u2 ++
    methodAttributes(method, cpIndex)

  private def methodAccess(method: JVMMethod) =
    if (method.static) publicStaticAccess
    else publicAccess

  private def methodAttributes(method: JVMMethod, cpIndex: ConstantPoolIndex) =
    methodAttributesLength ++
    methodCodeAttribute(method, cpIndex)

  private def methodAttributesLength = 1.u2

  /* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.3 */
  private def methodCodeAttribute(method: JVMMethod, cpIndex: ConstantPoolIndex) = {
    val body = methodCodeAttributeBody(method, cpIndex)

    cpIndex(Utf8Ref("Code")).u2 ++
    body.length.u4 ++
    body
  }

  private def methodCodeAttributeBody(method: JVMMethod, cpIndex: ConstantPoolIndex) = {
    val code = methodCode(method, cpIndex)

    method.maxStack.u2 ++
    method.maxLocals.u2 ++
    code.length.u4 ++
    code ++
    exceptionTable ++
    methodAttributesTable
  }

  private def methodCode(method: JVMMethod, cpIndex: ConstantPoolIndex) =
    InstructionTable.mkBytes(method.code, cpIndex)

  private def exceptionTable = exceptionTableLength
  private def exceptionTableLength = 0.u2

  private def methodAttributesTable = methodAttributesTableLength
  private def methodAttributesTableLength = 0.u2

  private def attributesTable = attributesTableLength
  private def attributesTableLength = 0.u2
}
