type SymbolTable = Map[String, ClassTable]
case class ClassTable(name: String, methods: Map[String, MethodTable], fields: Map[String, Type])
case class MethodTable(name: String, returnType: Type, params: Map[String, Type], locals: Map[String, Type])

object SymbolTableCreator {
  def create(program: Program): SymbolTable = {
    groupByNameC(createClassTable(program.mainClass) +: program.classDecls.map(createClassTable))
  }

  def createClassTable(mainClass: MainClass): ClassTable =
    ClassTable(name = mainClass.name.name,
      methods = Map("main" -> MethodTable("main", Void(), Map(), createVarDeclMap(mainClass.varDecls))),
      fields = Map())

  def createClassTable(classDecl: ClassDecl): ClassTable =
    ClassTable(name = classDecl.name.name,
      methods = groupByName(classDecl.methodDecls.map(createMethodTable)),
      fields = createVarDeclMap(classDecl.varDecls))

  def createMethodTable(methodDecl: MethodDecl): MethodTable =
    MethodTable(name = methodDecl.name.name,
      returnType = methodDecl.typeName,
      params = createFormalMap(methodDecl.argList),
      locals = createVarDeclMap(methodDecl.varDeclList))

  def createVarDeclMap(formals: Seq[VarDecl]): Map[String, Type] =
    formals.map(f => (f.name.name, f.typeName)).toMap

  def createFormalMap(formals: Seq[Formal]): Map[String, Type] =
    formals.map(f => (f.name.name, f.typeName)).toMap

  def groupByName(methodTables: Seq[MethodTable]): Map[String, MethodTable] =
    methodTables.groupBy(_.name).mapValues(_.head)

  def groupByNameC(classTables: Seq[ClassTable]): Map[String, ClassTable] =
    classTables.groupBy(_.name).mapValues(_.head)

  // TODO: handle duplicates

}
