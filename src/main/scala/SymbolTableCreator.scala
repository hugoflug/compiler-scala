import EitherUtils.orFirstError

object SymbolTableCreator {
  type SymbolTable = Map[String, ClassTable]

  case class ClassTable(name: String, methods: Map[String, MethodTable], fields: Map[String, Type])
  case class MethodTable(name: String, returnType: Type, params: Map[String, Type], locals: Map[String, Type])

  case class RedefinitionError(name: String) extends CompilerError

  def create(program: Program): Either[RedefinitionError, SymbolTable] =
    for {
      classTable <- orFirstError(program.classDecls.map(createClassTable))
      mainClassTable <- createClassTable(program.mainClass)
      symbolTable <- dedup((mainClassTable +: classTable).groupBy(_.name))
    } yield symbolTable

  private type R[A] = Either[RedefinitionError, A]
  
  private def createClassTable(mainClass: MainClass): R[ClassTable] =
    for {
      varDecls <- dedup(createVarDeclMap(mainClass.varDecls))
      name = mainClass.name.name
      methods = Map("main" -> MethodTable("main", Void(), Map(), varDecls))
      fields: Map[String, Type] = Map()
    } yield ClassTable(name, methods, fields)

  private def createClassTable(classDecl: ClassDecl): R[ClassTable] =
    for {
      fields <- dedup(createVarDeclMap(classDecl.varDecls))
      methodTables <- orFirstError(classDecl.methodDecls.map(createMethodTable(_, fields.keys.toSeq)))
      methods <- dedup(methodTables.groupBy(_.name))
      name = classDecl.name.name
    } yield ClassTable(name, methods, fields)

  private def createMethodTable(methodDecl: MethodDecl, fieldNames: Seq[String]): R[MethodTable] =
    for {
      params <- dedup(createFormalMap(methodDecl.argList))
      locals <- dedup(createVarDeclMap(methodDecl.varDeclList))
      name = methodDecl.name.name
      returnType = methodDecl.typeName
      _ <- assertNoDuplicates(params.keys ++ locals.keys ++ fieldNames)
    } yield MethodTable(name, returnType, params, locals)

  private def createVarDeclMap(formals: Seq[VarDecl]): Map[String, Seq[Type]] =
    toMultiValuedMap(formals.map(v => (v.name.name, v.typeName)))

  private def createFormalMap(formals: Seq[Formal]): Map[String, Seq[Type]] =
    toMultiValuedMap(formals.map(f => (f.name.name, f.typeName)))

  private def toMultiValuedMap[A, B](seq: Seq[(A, B)]): Map[A, Seq[B]] =
    seq.groupBy(_._1).mapValues(v => v.map(_._2))

  private def assertNoDuplicates[A, B](it: Iterable[A]): R[Map[A, A]] =
    dedup(it.groupBy(a => a).mapValues(_.toSeq))

  private def dedup[A, B](map: Map[A, Seq[B]]): R[Map[A, B]] =
    map.find({ case (_, v) => v.length > 1 }) match {
      case Some((key, _)) => Left(RedefinitionError(key.toString))
      case None => Right(map.mapValues(_.head))
    }
}
