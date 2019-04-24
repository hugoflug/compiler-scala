import EitherUtils.orFirstError
import TypeChecker.{Type, Void}

object SymbolTableCreator {
  type SymbolTable = Map[String, ClassTable]

  case class ClassTable(name: String, methods: Map[String, MethodTable], fields: Map[String, Var])
  case class MethodTable(name: String, returnType: Type, params: Map[String, Var], locals: Map[String, Var])
  case class Var(name: String, type_ : Type, varNo: Int)

  case class RedefinitionError(name: String, override val index: Int) extends CompilationError(index)

  def create(program: Program): Either[RedefinitionError, SymbolTable] =
    for {
      classTable <- orFirstError(program.classDecls.map(createClassTable))
      mainClassTable <- createClassTable(program.mainClass)
      symbolTable <- dedup((mainClassTable +: classTable).groupBy(_.name))
    } yield symbolTable

  private type R[A] = Either[RedefinitionError, A]
  
  private def createClassTable(mainClass: MainClass): R[ClassTable] =
    for {
      varDecls <- dedup(createVarMap(mainClass.varDecls))
      name = mainClass.name.name
      methods = Map("main" -> MethodTable("main", Void(), Map(), varDecls))
      fields = Map[String, Var]()
    } yield ClassTable(name, methods, fields)

  private def createClassTable(classDecl: ClassDecl): R[ClassTable] =
    for {
      fields <- dedup(createVarMap(classDecl.varDecls))
      methodTables <- orFirstError(classDecl.methodDecls.map(createMethodTable))
      methods <- dedup(methodTables.groupBy(_.name))
      name = classDecl.name.name
    } yield ClassTable(name, methods, fields)

  private def createMethodTable(methodDecl: MethodDecl): R[MethodTable] =
    for {
      params <- dedup(createVarMap(methodDecl.argList))
      locals <- dedup(createVarMap(methodDecl.varDeclList))
      name = methodDecl.name.name
      returnType = methodDecl.typeName
      _ <- assertNoDuplicates(params.keys.toSeq ++ locals.keys.toSeq)
    } yield MethodTable(name, TypeChecker.typeOfNode(returnType), params, locals)

  private def createVarMap(genVarDecls: Seq[GenVarDecl]): Map[String, Seq[Var]] =
    genVarDecls.zipWithIndex.map({ case(v, i) =>
      Var(v.name.name, TypeChecker.typeOfNode(v.typeName), i) }).groupBy(_.name)

  private def assertNoDuplicates[A](it: Iterable[A]): R[Map[A, A]] =
    dedup(it.groupBy(a => a).mapValues(_.toSeq))

  private def dedup[A, B](map: Map[A, Seq[B]]): R[Map[A, B]] =
    map.find({ case (_, v) => v.length > 1 }) match {
      case Some((key, _)) => Left(RedefinitionError(key.toString, 0 /*TODO*/))
      case None => Right(map.mapValues(_.head))
    }
}
