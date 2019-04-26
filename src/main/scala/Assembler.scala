object Assembler {
  case class ClassFile(filename: String, content: Array[Byte])

  case class ClassRef(index: Int, nameIndex: Int)
  case class MethodRef(index: Int, classIndex: Int, nameAndTypeIndex: Int)
  case class FieldRef(index: Int, classIndex: Int, nameAndTypeIndex: Int)
  case class NameAndTypeRef(index: Int, nameIndex: Int, typeIndex: Int)

  case class NameAndTypeKey(name: String, typeDesc: String)
  case class MethodKey(clazz: String, name: String, typeDesc: String)
  case class FieldKey(clazz: String, name: String, typeDesc: String)

  case class ConstantPoolInfo(ints: Map[Int, Int], utf8Strings: Map[String, Int], classRefs: Map[String, ClassRef],
                              nameAndTypeRefs: Map[NameAndTypeKey, NameAndTypeRef],
                              fieldRefs: Map[FieldKey, FieldRef], methodRefs: Map[MethodKey, MethodRef]) {

    def size = ints.size + utf8Strings.size + classRefs.size + nameAndTypeRefs.size + fieldRefs.size + methodRefs.size

    def getOrAddInt(i: Int): (ConstantPoolInfo, Int) =
      ints.get(i) match {
        case Some(index) => (this, index)
        case None => (this.copy(ints = ints + (i -> size)), size)
      }

    def getOrAddString(s: String): (ConstantPoolInfo, Int) =
      utf8Strings.get(s) match {
        case Some(index) => (this, index)
        case None => (this.copy(utf8Strings = utf8Strings + (s -> size)), size)
      }

    def addClassRef(key: String, ref: ClassRef): ConstantPoolInfo =
      this.copy(classRefs = classRefs + (key -> ref))

    def getOrAddClassRef(c: String): (ConstantPoolInfo, Int) =
      classRefs.get(c) match {
        case Some(cr) => (this, cr.index)
        case None =>
          val (cp1, nameIndex) = getOrAddString(c)
          (cp1.copy(classRefs = classRefs + (c -> ClassRef(size, nameIndex))),
            size)
      }

    def addNameAndTypeRef(key: NameAndTypeKey, ref: NameAndTypeRef): ConstantPoolInfo =
      this.copy(nameAndTypeRefs = nameAndTypeRefs + (key -> ref))

    def getOrAddNameAndTypeRef(key: NameAndTypeKey): (ConstantPoolInfo, Int) =
      nameAndTypeRefs.get(key) match {
        case Some(ref) => (this, ref.index)
        case None =>
          val (cp1, nameIndex) = this.getOrAddString(key.name)
          val (cp2, typeIndex) = cp1.getOrAddString(key.typeDesc)
          (cp2.addNameAndTypeRef(key, NameAndTypeRef(size, nameIndex, typeIndex)), size)
      }

    def addFieldRef(key: FieldKey, ref: FieldRef): ConstantPoolInfo =
      this.copy(fieldRefs = fieldRefs + (key -> ref))

    def getOrAddFieldRef(key: FieldKey): (ConstantPoolInfo, Int) =
      fieldRefs.get(key) match {
        case Some(ref) => (this, ref.index)
        case None =>
          val (cp1, classIndex) = this.getOrAddClassRef(key.clazz)
          val (cp2, nameAndTypeIndex) = cp1.getOrAddNameAndTypeRef(NameAndTypeKey(key.name, key.typeDesc))
          (cp2.addFieldRef(key, FieldRef(size, classIndex, nameAndTypeIndex)), size)
      }
  }

  def findConstantPoolInfo(classAssembly: ClassAssembly) = {
    val info = ConstantPoolInfo(Map(), Map(), Map(), Map(), Map(), Map())
    val newInfo = info.getOrAddString(classAssembly.className)
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
