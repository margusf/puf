package puf

import ast._

/** Finds all the free variables in an expression. */
object FreeVars {
    def get(expr: Expr): Set[String] = expr match {
        case Num(_) => Set.empty
        case Id(txt) => Set(txt)
        case Letrec(decls, expr) =>
            val leftVars = decls.flatMap(getDeclVars).toSet
            val rightVars = decls.map(_.right).flatMap(get).toSet
            rightVars ++ get(expr) -- leftVars
        case Let(decls, expr) => Set.empty // TODO
        case Lambda(params, expr) =>
            val paramVars = params.map(_.text)
            get(expr) -- paramVars
        case If(cond, ifExpr, elseExpr) => 
            get(cond) ++ get(ifExpr) ++ get(elseExpr)
        case Binary(_, left, right) => get(left) ++ get(right)
        case Unary(_, expr) => get(expr)
        case Apply(fun, params) => get(fun) ++ params.flatMap(get)
        case _ =>
            throw new Exception("Unsupported expression: " + expr)
    }
    
    def getDeclVars(decl: Decl) = decl.left match {
        case Id(text) =>
            Set(text)
        case TupleLeft(items) =>
            items.map(_.text).toSet
    }
}