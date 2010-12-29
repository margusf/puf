package puf

// Helper for sorting declarations in a letrec expression, according
// to their depencencies.

import ast._
import collection.mutable.ArrayBuffer

object LetrecHelper {
    /**Checks that the variable declarations in letrec do not contain
      * circular references. Also, sorts the declarations according to
      * dependencies. */
    def checkLetrecDecls(decls: List[Decl]) = {
        val dependencies = getDependencies(decls)
        // Sort the dependencies topologically.
        val sorted = sortDependencies(dependencies)
        // Reorder the declarations so that they match the order
        // of sorted dependencies.
        val declMap = decls.map((x: Decl) => (getVar(x), x)).toMap
        sorted.map(declMap(_))
    }

    def letrecMappings(decls: List[Decl], sd: Int) = {
        val idList = decls.map(getVar)
        idList.zip(Range(sd + 1, sd + idList.size + 1)).toMap
    }

    private def getDependencies(decls: List[Decl]) = {
        // Set of all variables in the declarations
        val declVars = decls.map(getVar).toSet

        def processDecl(d: Decl) = {
            // All free variables in the RHS of a declaration.
            val allDeps = FreeVars.get(d.right, true)
            // We are interested only in dependencies to other variables
            // in the same let-expression. Assume that the global variables
            // can manage themselves.
            val realDeps = allDeps.intersect(declVars)
            (getVar(d), realDeps)
        }

        decls.map(processDecl).toMap
    }

    private def sortDependencies(deps: Map[String, Set[String]]) = {
        // Contains identifiers in
        val ret = new ArrayBuffer[String]
        var remainingDeps = deps

        while (!remainingDeps.isEmpty) {
            val goodDep = remainingDeps.find(
                (x) => x._2.isEmpty)
            goodDep match {
                case Some((id, _)) =>
                    ret += id
                    remainingDeps -= id
                    // Remove dependency to newly removed variable.
                    remainingDeps = removeDependency(remainingDeps, id)
                case None =>
                    val (bId, bDeps) = remainingDeps.head
                    val badVars = (bId :: bDeps.toList).mkString(", ")
                    throw new Exception(
                        "Unresolvable circular dependency between variables " +
                        badVars)
            }
        }

        ret.toList
    }

    /** Removes dependency to given identifier. */
    private def removeDependency(deps: Map[String, Set[String]], id: String) =
        deps.mapValues(_ - id)

    private def getVar(decl: Decl) =
        decl.left match {
            case Id(name) =>
                name
            case TupleLeft(_) =>
                throw new Exception("Tuples are not supported in letrec")
    }
}