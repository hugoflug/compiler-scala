type Redef[A] = Either[RedefinitionError, A]
type SymbolTable = Map[String, ClassTable]
case class ClassTable(name: String, methods: Map[String, MethodTable], fields: Map[String, Type])
case class MethodTable(name: String, returnType: Type, params: Map[String, Type], locals: Map[String, Type])
case class RedefinitionError(msg: String)

object SymbolTableCreator {
  def create(program: Program): Redef[SymbolTable] =
    for {
      classTable <- firstError(program.classDecls.map(createClassTable))
      mainClassTable <- createClassTable(program.mainClass)
      symbolTable <- groupByNameC(mainClassTable +: classTable)
    } yield symbolTable

  def createClassTable(mainClass: MainClass): Redef[ClassTable] =
    for {
      varDecls <- createVarDeclMap(mainClass.varDecls)
      name = mainClass.name.name
      methods = Map("main" -> MethodTable("main", Void(), Map(), varDecls))
      fields: Map[String, Type] = Map()
    } yield ClassTable(name, methods, fields)

  def createClassTable(classDecl: ClassDecl): Redef[ClassTable] =
    for {
      methodTables <- firstError(classDecl.methodDecls.map(createMethodTable))
      methods <- groupByName(methodTables)
      fields <- createVarDeclMap(classDecl.varDecls)
      name = classDecl.name.name
    } yield ClassTable(name, methods, fields)

  def createMethodTable(methodDecl: MethodDecl): Redef[MethodTable] =
    for {
      params <- createFormalMap(methodDecl.argList)
      locals <- createVarDeclMap(methodDecl.varDeclList)
      name = methodDecl.name.name
      returnType = methodDecl.typeName
    } yield MethodTable(name, returnType, params, locals)

  def createVarDeclMap(formals: Seq[VarDecl]): Redef[Map[String, Type]] =
    dedup(formals, (v: VarDecl) => (v.name.name, v.typeName))

  def createFormalMap(formals: Seq[Formal]): Redef[Map[String, Type]] =
    dedup(formals, (f: Formal) => (f.name.name, f.typeName))

  def groupByName(methodTables: Seq[MethodTable]): Redef[Map[String, MethodTable]] =
    dedup(methodTables.groupBy(_.name))

  def groupByNameC(classTables: Seq[ClassTable]): Redef[Map[String, ClassTable]] =
    dedup(classTables.groupBy(_.name))

  def dedup[A, B, C](seq: Seq[A], fn: A => (B, C)) : Redef[Map[B, C]] = {
    dedup(seq.map(fn).groupBy(_._1).mapValues(v => v.map(_._2)))
  }

  def dedup[A, B](map: Map[A, Seq[B]]): Redef[Map[A, B]] = {
    val dups = map.filter({ case (_, v) => v.length > 1 })
    if (dups.nonEmpty) Left(RedefinitionError("Duplicate key: " + dups.head._1))
    else Right(map.mapValues(_.head))
  }

  def firstError[A, B](eithers: Seq[Either[A, B]]): Either[A, Seq[B]] = {
    val lefts = eithers.collect({ case left: Left[A, B] => left })
    if (lefts.nonEmpty) Left(lefts.head.left.get)
    else Right(eithers.map(_.right.get))
  }
}