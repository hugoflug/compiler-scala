object ConstantPoolUtil {
  trait ConstantPoolEntry {
    def index: Int
    def ref: ConstantPoolRef
  }
  case class IntEntry(ref: IntRef, index: Int) extends ConstantPoolEntry
  case class StringEntry(ref: StringRef, index: Int) extends ConstantPoolEntry
  case class ClassEntry(ref: ClassRef, index: Int, nameIndex: Int) extends ConstantPoolEntry
  case class MethodEntry(ref: MethodRef, index: Int, classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class FieldEntry(ref: FieldRef, index: Int, classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class NatEntry(ref: NatRef, index: Int, nameIndex: Int, typeIndex: Int) extends ConstantPoolEntry

  private trait ConstantPoolRef
  case class ClassRef(name: String) extends ConstantPoolRef
  case class StringRef(value: String) extends ConstantPoolRef
  case class IntRef(value: Int) extends ConstantPoolRef
  case class NatRef(name: String, typeDesc: String) extends ConstantPoolRef
  case class MethodRef(clazz: String, name: String, typeDesc: String) extends ConstantPoolRef
  case class FieldRef(clazz: String, name: String, typeDesc: String) extends ConstantPoolRef

  def constantPoolEntries(clazz: ClassAssembly): Set[ConstantPoolEntry] =
    constantPoolEntries(constantPoolRefs(clazz))

  private def constantPoolRefs(clazz: ClassAssembly): Set[ConstantPoolRef] = ???

  private def createFieldEntry(ref: FieldRef, index: Int, classEntries: Set[ClassEntry],
                       natEntries: Set[NatEntry]): FieldEntry = {
    val classInfoIndex = classEntries.find(_.ref.name == ref.clazz).get.index
    val natInfoIndex = natEntries.find(n => n.ref.name == ref.name && n.ref.typeDesc == ref.typeDesc).get.index
    FieldEntry(ref, index, classInfoIndex, natInfoIndex)
  }

  private def createMethodEntry(ref: MethodRef, index: Int, classEntries: Set[ClassEntry],
                        natEntries: Set[NatEntry]): MethodEntry = {
    val classInfoIndex = classEntries.find(_.ref.name == ref.clazz).get.index
    val natInfoIndex = natEntries.find(n => n.ref.name == ref.name && n.ref.typeDesc == ref.typeDesc).get.index
    MethodEntry(ref, index, classInfoIndex, natInfoIndex)
  }

  private def createNatEntry(ref: NatRef, index: Int, stringEntries: Set[StringEntry]): NatEntry =
    NatEntry(ref, index, stringEntries.find(_.ref.value == ref.name).get.index,
      stringEntries.find(_.ref.value == ref.typeDesc).get.index)

  private def constantPoolEntries(refs: Set[ConstantPoolRef]): Set[ConstantPoolEntry] = {
    val intRefs = refs.collect({ case i: IntRef => i })
    val stringRefs = refs.collect({ case i: StringRef => i })
    val classRefs = refs.collect({ case i: ClassRef => i })
    val fieldRefs = refs.collect({ case i: FieldRef => i })
    val methodRefs = refs.collect({ case i: MethodRef => i })

    val intEntries = intRefs.zipWithIndex
      .map({ case(ref, index) => IntEntry(ref, index) })
    val startIndex1 = intEntries.size

    val strings = stringRefs ++
      classRefs.map(r => StringRef(r.name)) ++
      fieldRefs.map(r => StringRef(r.clazz)) ++
      fieldRefs.map(r => StringRef(r.name)) ++
      fieldRefs.map(r => StringRef(r.typeDesc)) ++
      methodRefs.map(r => StringRef(r.clazz)) ++
      methodRefs.map(r => StringRef(r.name)) ++
      methodRefs.map(r => StringRef(r.typeDesc))
    val stringEntries = strings.zipWithIndex
      .map(a => (a._1, a._2 + startIndex1))
      .map({ case (ref, index) => StringEntry(ref, index) })
    val startIndex2 = startIndex1 + stringEntries.size

    val classEntries = classRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex2))
      .map({ case (ref, index) => ClassEntry(ref, index, stringEntries.find(_.ref.value == ref.name).get.index) })
    val startIndex3 = startIndex2 + classEntries.size

    val natRefs =
      fieldRefs.map(f => NatRef(f.name, f.typeDesc)) ++
        methodRefs.map(f => NatRef(f.name, f.typeDesc))
    val natEntries = natRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex3))
      .map({ case(ref, index) => createNatEntry(ref, index, stringEntries) })
    val startIndex4 = startIndex3 + natEntries.size

    val methodEntries = methodRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex4))
      .map({ case(ref, index) => createMethodEntry(ref, index, classEntries, natEntries) })

    val startIndex5 = startIndex4 + methodRefs.size

    val fieldEntries = fieldRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex5))
      .map({ case(ref, index) => createFieldEntry(ref, index, classEntries, natEntries) })

    intEntries ++ stringEntries ++ classEntries ++ natEntries ++ methodEntries ++ fieldEntries
  }
}
