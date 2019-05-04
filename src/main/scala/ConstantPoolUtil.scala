object ConstantPoolUtil {
  sealed trait ConstantPoolEntry {
    def index: Int
    def ref: ConstantPoolRef
  }
  case class IntEntry(ref: IntRef, index: Int) extends ConstantPoolEntry
  case class Utf8Entry(ref: Utf8Ref, index: Int) extends ConstantPoolEntry
  case class StringEntry(ref: StringRef, index: Int, utf8Index: Int) extends ConstantPoolEntry
  case class ClassEntry(ref: ClassRef, index: Int, nameIndex: Int) extends ConstantPoolEntry
  case class MethodEntry(ref: MethodRef, index: Int, classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class FieldEntry(ref: FieldRef, index: Int, classIndex: Int, nameAndTypeIndex: Int) extends ConstantPoolEntry
  case class NatEntry(ref: NatRef, index: Int, nameIndex: Int, typeIndex: Int) extends ConstantPoolEntry

  sealed trait ConstantPoolRef
  case class StringRef(value: String) extends ConstantPoolRef
  case class ClassRef(name: String) extends ConstantPoolRef
  case class Utf8Ref(value: String) extends ConstantPoolRef
  case class IntRef(value: Int) extends ConstantPoolRef
  case class NatRef(name: String, typeDesc: String) extends ConstantPoolRef
  case class MethodRef(clazz: String, name: String, typeDesc: String) extends ConstantPoolRef
  case class FieldRef(clazz: String, name: String, typeDesc: String) extends ConstantPoolRef

  def constantPoolEntries(clazz: JVMClass): Set[ConstantPoolEntry] =
    constantPoolEntries(constantPoolRefs(clazz))

  private def constantPoolRefs(clazz: JVMClass): Set[ConstantPoolRef] =
    Set[ConstantPoolRef]() +
    ClassRef(clazz.className) +
    ClassRef(clazz.superClass) ++
    clazz.fields.map(f => FieldRef(clazz.className, f.name, f.typeDesc)).toSet ++
    clazz.methods.map(m => MethodRef(clazz.className, m.name, m.typeDesc)).toSet ++
    clazz.methods.flatMap(m => codeConstantPoolRefs(m.code))

  private def codeConstantPoolRefs(instructions: Seq[JVMInstruction]): Set[ConstantPoolRef] =
    instructions.map(constantPoolRef).filter(_.isDefined).map(_.get).toSet

  private def constantPoolRef(instruction: JVMInstruction): Option[ConstantPoolRef] =
    instruction match {
      case instr: InstructionWithFieldRef =>
        Some(FieldRef(instr.clazz, instr.name, instr.typeDesc))
      case instr: InstructionWithMethodRef =>
        Some(MethodRef(instr.clazz, instr.name, instr.typeDesc))
      case Ldc_wInt(value) =>
        Some(IntRef(value))
      case Ldc_wString(s) =>
        Some(StringRef(s))
      case New(clazz) =>
        Some(ClassRef(clazz))
      case _ =>
        None
    }


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

  private def createNatEntry(ref: NatRef, index: Int, utf8Entries: Set[Utf8Entry]): NatEntry =
    NatEntry(ref, index, utf8Entries.find(_.ref.value == ref.name).get.index,
      utf8Entries.find(_.ref.value == ref.typeDesc).get.index)

  /*private*/ def constantPoolEntries(refs: Set[ConstantPoolRef]): Set[ConstantPoolEntry] = {
    val intRefs = refs.collect({ case i: IntRef => i })
    val utf8Refs = refs.collect({ case i: Utf8Ref => i })
    val stringRefs = refs.collect({ case i: StringRef => i })
    val classRefs = refs.collect({ case i: ClassRef => i })
    val fieldRefs = refs.collect({ case i: FieldRef => i })
    val methodRefs = refs.collect({ case i: MethodRef => i })

    val intEntries = intRefs.zipWithIndex
      .map({ case(ref, index) => IntEntry(ref, index + 1) })
    val startIndex1 = intEntries.size + 1

    val utf8s = utf8Refs ++
      stringRefs.map(r => Utf8Ref(r.value)) ++
      classRefs.map(r => Utf8Ref(r.name)) ++
      fieldRefs.map(r => Utf8Ref(r.clazz)) ++
      fieldRefs.map(r => Utf8Ref(r.name)) ++
      fieldRefs.map(r => Utf8Ref(r.typeDesc)) ++
      methodRefs.map(r => Utf8Ref(r.clazz)) ++
      methodRefs.map(r => Utf8Ref(r.name)) ++
      methodRefs.map(r => Utf8Ref(r.typeDesc)) +
      Utf8Ref("Code")
    val utf8Entries = utf8s.zipWithIndex
      .map(a => (a._1, a._2 + startIndex1))
      .map({ case (ref, index) => Utf8Entry(ref, index) })
    val startIndex2 = startIndex1 + utf8Entries.size

    val stringEntries = stringRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex2))
      .map({ case(ref, index) => StringEntry(ref, index, utf8Entries.find(_.ref.value == ref.value).get.index) })
    val startIndex3 = startIndex2 + stringEntries.size

    val classes = classRefs ++
      fieldRefs.map(r => ClassRef(r.clazz)) ++
      methodRefs.map(r => ClassRef(r.clazz))
    val classEntries = classes.zipWithIndex
      .map(a => (a._1, a._2 + startIndex3))
      .map({ case (ref, index) => ClassEntry(ref, index, utf8Entries.find(_.ref.value == ref.name).get.index) })
    val startIndex4 = startIndex3 + classEntries.size

    val natRefs =
      fieldRefs.map(f => NatRef(f.name, f.typeDesc)) ++
        methodRefs.map(f => NatRef(f.name, f.typeDesc))
    val natEntries = natRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex4))
      .map({ case(ref, index) => createNatEntry(ref, index, utf8Entries) })
    val startIndex5 = startIndex4 + natEntries.size

    val methodEntries = methodRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex5))
      .map({ case(ref, index) => createMethodEntry(ref, index, classEntries, natEntries) })
    val startIndex6 = startIndex5 + methodRefs.size

    val fieldEntries = fieldRefs.zipWithIndex
      .map(a => (a._1, a._2 + startIndex6))
      .map({ case(ref, index) => createFieldEntry(ref, index, classEntries, natEntries) })

    intEntries ++ utf8Entries ++ stringEntries ++ classEntries ++ natEntries ++ methodEntries ++ fieldEntries
  }
}
