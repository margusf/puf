package puf

import ast._

object TailCall {
    def markCalls(expr: Expr) {
        markCalls(expr, None)
    }

    def markCalls(expr: Expr, tailPosition: Option[Int]) {
        println("markCalls(" + tailPosition + ", " + expr + ")")
        expr match {
            case Apply(f, params) =>
                expr.asInstanceOf[Apply].tailPosition = tailPosition
                markCalls(f, None)
                params.foreach(markCalls(_, None))
            case Binary(_, left, right) =>
                markCalls(left, None)
                markCalls(right, None)
            case Case(cond, nilExpr, ConsAlt(_, _, consExpr)) =>
                markCalls(cond, None)
                markCalls(nilExpr, tailPosition)
                markCalls(consExpr, tailPosition)
            case Cons(left, right) =>
                markCalls(left, None)
                markCalls(right, None)
            case If(cond, ifExpr, elseExpr) =>
                markCalls(cond, None)
                markCalls(ifExpr, tailPosition)
                markCalls(elseExpr, tailPosition)
            case Let(decls, expr) =>
                markDecls(decls)
                markCalls(expr, tailPosition)
            case Letrec(decls, expr) =>
                markDecls(decls)
                markCalls(expr, tailPosition)
            case Lambda(params, body) =>
                markCalls(body, Some(params.length))
            case TupleLit(items) =>
                items.foreach(markCalls(_, None))
            case Select(Num(idx), expr) =>
                markCalls(expr, None)
            case _ => ()
        }
    }

    private def markDecls(decls: List[Decl]) {
        for (Decl(_, right) <- decls) {
            markCalls(right, None)
        }
    }
}