// package wacc.backend

// import wacc.AST.Func
// import scala.collection.mutable
// import scala.collection.mutable.ListBuffer

// object FunctionDeclarationMap {

//     var functionReferenceMap: Map[String, Func] = Map.empty

//     private val funcMap = mutable.HashMap.empty[String, ListBuffer[Instruction]]
//     private val compiledFunctions = mutable.HashSet.empty[String]

//     def setFuncAsCompiled(name: String): Unit = compiledFunctions += name
//     def includeFuncBodyDef(name: String, body: ListBuffer[Instruction]): Unit = funcMap += name -> body
//     def isDefinedFunc(name: String): Boolean = compiledFunctions.contains(name)

//     def getAllCompiledFuncDefs: List[ListBuffer[Instruction]] = {
//         (for ((name, funcBody) <- funcMap) yield {
//             Label(name)
//             + Push(List(LR))
//             +: funcBody
//             +: Pop(List(PC))
//             +: Directive(".ltorg")
//         }).toList
//     }

// }
