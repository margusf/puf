package puf

import ast._

/** Finds all the free variables in an expression. */
object FreeVars {
    /** Return free variables in an expression.
      * @param eagerOnly if True, then return only variables that
      * are used immediately and ignore variables that are used
      * inside lambda-expressions.
      */
    def get(expr: Expr, eagerOnly: Boolean): Set[String] = expr match {
        case Num(_) => Set.empty
        case Id(txt) => Set(txt)
        case Letrec(decls, expr) =>
            getLetVars(decls, expr, eagerOnly)
        case Let(decls, expr) =>
            getLetVars(decls, expr, eagerOnly)
        case Lambda(params, expr) if !eagerOnly =>
            val paramVars = params.map(_.text)
            get(expr, eagerOnly) -- paramVars
        case Lambda(_, _) =>
            Set.empty
        case If(cond, ifExpr, elseExpr) => 
            get(cond, eagerOnly) ++
                    get(ifExpr, eagerOnly) ++ 
                    get(elseExpr, eagerOnly)
        case Binary(_, left, right) => 
            get(left, eagerOnly) ++ get(right, eagerOnly)
        case Unary(_, expr) => get(expr, eagerOnly)
        case Apply(fun, params) => 
            get(fun, eagerOnly) ++ params.flatMap(get(_, eagerOnly))
        case _ =>
            throw new Exception("Unsupported expression: " + expr)
    }

    def getLetVars(decls: List[Decl], expr: Expr, eagerOnly: Boolean) = {
        val leftVars = decls.flatMap(getDeclVars).toSet
        val rightVars = decls.map(_.right).flatMap(get(_, eagerOnly)).toSet
        rightVars ++ get(expr, eagerOnly) -- leftVars
    }

    def getDeclVars(decl: Decl) = decl.left match {
        case Id(text) =>
            Set(text)
        case TupleLeft(items) =>
            items.map(_.text).toSet
    }
}