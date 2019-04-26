object Assembler {
  case class ClassFile(filename: String, content: Array[Byte])

  case class ClassRef(index: Int, nameIndex: Int)
  case class MethodRef(index: Int, classIndex: Int, nameAndTypeIndex: Int)
  case class FieldRef(index: Int, classIndex: Int, nameAndTypeIndex: Int)
  case class NatRef(index: Int, nameIndex: Int, typeIndex: Int)

  case class NatKey(name: String, typeDesc: String)
  case class MethodKey(clazz: String, name: String, typeDesc: String)
  case class FieldKey(clazz: String, name: String, typeDesc: String)

  case class ConstantPoolKeys(methodKeys: Seq[MethodKey], fieldKeys: Seq[FieldKey], stringKeys: Seq[String],
                              intKeys: Seq[Int], classKeys: Seq[String])

  case class ConstantPoolInfo(ints: Map[Int, Int], utf8Strings: Map[String, Int], classRefs: Map[String, ClassRef],
                              nameAndTypeRefs: Map[NatKey, NatRef], fieldRefs: Map[FieldKey, FieldRef],
                              methodRefs: Map[MethodKey, MethodRef])


  def fieldKeyToRef(key: FieldKey, index: Int, classes: Map[String, ClassRef], nats: Map[NatKey, NatRef]): FieldRef =
    FieldRef(index, classes(key.clazz).index, nats(NatKey(key.name, key.typeDesc)).index)

  def methodKeyToRef(key: MethodKey, index: Int, classes: Map[String, ClassRef], nats: Map[NatKey, NatRef]): MethodRef =
    MethodRef(index, classes(key.clazz).index, nats(NatKey(key.name, key.typeDesc)).index)

  def natKeyToRef(key: NatKey, index: Int, strings: Map[String, Int]): NatRef =
    NatRef(index, strings(key.name), strings(key.typeDesc))

  def findConstantPoolInfo(c: ConstantPoolKeys): ConstantPoolInfo = {

    val ints = c.intKeys.zipWithIndex.toMap
    val startIndex1 = ints.size

    val strings = c.stringKeys ++
      c.classKeys ++
      c.fieldKeys.map(_.clazz) ++
      c.fieldKeys.map(_.name) ++
      c.fieldKeys.map(_.typeDesc) ++
      c.methodKeys.map(_.clazz) ++
      c.methodKeys.map(_.name) ++
      c.methodKeys.map(_.typeDesc)
    val stringInfos = strings.zipWithIndex
      .map({ case(s, index) => (s, index + startIndex1) }).toMap
    val startIndex2 = startIndex1 + strings.size

    val classInfos = c.classKeys.zipWithIndex
      .map(a => (a._1, a._2 + startIndex2))
      .map({ case(key, index) => (key, ClassRef(index, stringInfos(key))) })
      .toMap
    val startIndex3 = startIndex2 + classInfos.size

    val natKeys =
      c.fieldKeys.map(f => NatKey(f.name, f.typeDesc)) ++
      c.methodKeys.map(f => NatKey(f.name, f.typeDesc))
    val natInfos = natKeys.zipWithIndex
      .map(a => (a._1, a._2 + startIndex3))
      .map({ case(key, index) => (key, natKeyToRef(key, index, stringInfos)) })
      .toMap
    val startIndex4 = startIndex3 + natInfos.size

    val methodInfos = c.methodKeys.zipWithIndex
      .map(a => (a._1, a._2 + startIndex4))
      .map({ case(key, index) => (key, methodKeyToRef(key, index, classInfos, natInfos)) })
      .toMap
    val startIndex5 = startIndex4 + methodInfos.size

    val fieldInfos = c.fieldKeys.zipWithIndex
      .map(a => (a._1, a._2 + startIndex5))
      .map({ case(key, index) => (key, fieldKeyToRef(key, index, classInfos, natInfos)) })
      .toMap

    ConstantPoolInfo(ints, stringInfos, classInfos, natInfos, fieldInfos, methodInfos)
  }


  def assemble(classAssembly: ClassAssembly): ClassFile = {
    magicNumber ++
    minorVersion ++
    majorVersion
  }

  def hex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  def magicNumber = hex("CAFEBABE")
  def minorVersion = hex("0003")
  def majorVersion = hex("002D")
}
