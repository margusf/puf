package puf

import ast._

object LetrecHelper {
    def checkLetrecDecls(decls: List[Decl]) = {
        val dependencies = getDependencies(decls)
        val sorted = sortDependencies(dependencies)

        decls
    }

    def letrecMappings(decls: List[Decl], sd: Int) = {
        val idList = decls.map(getVar)
        idList.zip(Range(sd + 1, sd + idList.size + 1)).toMap
    }

    def getDependencies(decls: List[Decl]) =
        decls.map((d: Decl) => (getVar(d), FreeVars.get(d.right, true))).toMap

    def sortDependencies(deps: Map[String, Set[String]]) = {
        println("dependecies to sort: " + deps)
        deps
    }

    def getVar(decl: Decl) = 
        decl.left match {
            case Id(name) =>
                name
            case TupleLeft(_) =>
                throw new Exception("Tuples are not supported in letrec")
    }
}