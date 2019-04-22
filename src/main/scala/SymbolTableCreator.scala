import EitherUtils.firstError

object SymbolTableCreator {
  type SymbolTable = Map[String, ClassTable]

  case class ClassTable(name: String, methods: Map[String, MethodTable], fields: Map[String, Type])
  case class MethodTable(name: String, returnType: Type, params: Map[String, Type], locals: Map[String, Type])

  case class RedefinitionError(msg: String) extends CompilerError

  // TODO: handle cross-type name conflicts, e.g. param with same name as local

  def create(program: Program): Either[RedefinitionError, SymbolTable] =
    for {
      classTable <- firstError(program.classDecls.map(createClassTable))
      mainClassTable <- createClassTable(program.mainClass)
      symbolTable <- groupByNameC(mainClassTable +: classTable)
    } yield symbolTable

  private type R[A] = Either[RedefinitionError, A]

  private def createClassTable(mainClass: MainClass): R[ClassTable] =
    for {
      varDecls <- createVarDeclMap(mainClass.varDecls)
      name = mainClass.name.name
      methods = Map("main" -> MethodTable("main", Void(), Map(), varDecls))
      fields: Map[String, Type] = Map()
    } yield ClassTable(name, methods, fields)

  private def createClassTable(classDecl: ClassDecl): R[ClassTable] =
    for {
      methodTables <- firstError(classDecl.methodDecls.map(createMethodTable))
      methods <- groupByName(methodTables)
      fields <- createVarDeclMap(classDecl.varDecls)
      name = classDecl.name.name
    } yield ClassTable(name, methods, fields)

  private def createMethodTable(methodDecl: MethodDecl): R[MethodTable] =
    for {
      params <- createFormalMap(methodDecl.argList)
      locals <- createVarDeclMap(methodDecl.varDeclList)
      name = methodDecl.name.name
      returnType = methodDecl.typeName
    } yield MethodTable(name, returnType, params, locals)

  private def createVarDeclMap(formals: Seq[VarDecl]): R[Map[String, Type]] =
    dedup(formals, (v: VarDecl) => (v.name.name, v.typeName))

  private def createFormalMap(formals: Seq[Formal]): R[Map[String, Type]] =
    dedup(formals, (f: Formal) => (f.name.name, f.typeName))

  private def groupByName(methodTables: Seq[MethodTable]): R[Map[String, MethodTable]] =
    dedup(methodTables.groupBy(_.name))

  private def groupByNameC(classTables: Seq[ClassTable]): R[Map[String, ClassTable]] =
    dedup(classTables.groupBy(_.name))

  private def dedup[A, B, C](seq: Seq[A], fn: A => (B, C)) : R[Map[B, C]] =
    dedup(seq.map(fn).groupBy(_._1).mapValues(v => v.map(_._2)))

  private def dedup[A, B](map: Map[A, Seq[B]]): R[Map[A, B]] = {
    val dups = map.filter({ case (_, v) => v.length > 1 })
    if (dups.nonEmpty) Left(RedefinitionError("Duplicate key: " + dups.head._1))
    else Right(map.mapValues(_.head))
  }
}
