package puf

object Desugar {
    def desugar(prog: spl.Program): ast.Expr =
        ast.Letrec(
                prog.decls.map(desugar),
                ast.Id("main"))

    def desugar(decl: spl.Decl): ast.Decl = decl match {
        case rd: spl.Rdecl => desugar(rd)
        case td: spl.Tdecl => desugar(td)
    }

    def desugar(decl: spl.Rdecl): ast.Decl = {
        val valExpr = desugar(decl.expr)

        decl.lhs.id match {
            case id :: Nil =>
                ast.Decl(desugar(id), valExpr)
            case fun :: args =>
                ast.Decl(desugar(fun),
                        ast.Lambda(args.map(desugar), valExpr))
            case _ =>
                throw new DesugarException(decl.lhs)
        }
    }

    def desugar(decl: spl.Tdecl): ast.Decl =
        ast.Decl(desugar(decl.lhs), desugar(decl.expr))

    def desugar(lhs: spl.Tlhs): ast.DeclLeft =
        ast.TupleLeft((lhs.h :: lhs.t).map(desugar))

    def desugar(expr: spl.Expr): ast.Expr = expr match {
        case c: spl.CaseExpr =>
            ast.Case(desugar(c.expr),
                    desugar(c.nilAlt.expr),
                    ast.ConsAlt(desugar(c.consAlt.head),
                            desugar(c.consAlt.tail),
                            desugar(c.consAlt.expr)))
        case lr: spl.LetrecExpr =>
            ast.Letrec(lr.decl.map(desugar),
                    desugar(lr.expr))
        case l: spl.LetExpr =>
            ast.Let(l.decl.map(desugar),
                    desugar(l.expr))
        case f: spl.FunExpr =>
            ast.Lambda(f.params.map(desugar), desugar(f.expr))
        case i: spl.IfExpr =>
            ast.If(desugar(i.cond),
                    desugar(i.ifThen),
                    desugar(i.ifElse))
        case o: spl.OrExpr =>
            splitLeftAssoc(o.left :: o.rest, ast.BinaryOp.Or)
        case _ =>
            throw new DesugarException(expr)
    }

    implicit def desugar(expr: spl.AndExpr): ast.Expr =
        splitLeftAssoc(expr.left :: expr.rest, ast.BinaryOp.And)

    implicit def desugar(expr: spl.EqExpr): ast.Expr =
        if (expr.right eq null)
            desugar(expr.left)
        else
            ast.Binary(ast.BinaryOp.withName(expr.op.text),
                    desugar(expr.left),
                    desugar(expr.right))

    def desugar(expr: spl.CompareExpr): ast.Expr =
        if (expr.right eq null)
            desugar(expr.left)
        else
            ast.Binary(ast.BinaryOp.withName(expr.op.text),
                    desugar(expr.left),
                    desugar(expr.right))

    def desugar(expr: spl.ConsExpr): ast.Expr =
        if (expr.right eq null)
            desugar(expr.left)
        else
            ast.Binary(ast.BinaryOp.Cons,
                    desugar(expr.left),
                    desugar(expr.right))

    def desugar(expr: spl.PlusExpr): ast.Expr = {
        def des(it: (String, spl.MulExpr)): (String, ast.Expr) =
            (it._1 , desugar(it._2))
        def cons(left: (String, ast.Expr),
                right: (String, ast.Expr)): (String, ast.Expr) =
            (right._1,
            ast.Binary(ast.BinaryOp.withName(right._1),
                    left._2,
                    right._2))

        val lst = ("", expr.left) :: expr.op.map(_.text).zip(expr.rest)
        val ret = splitLeftAssoc(lst, cons)(des)
        ret._2
    }

    def desugar(expr: spl.MulExpr): ast.Expr = {
        // TODO: this is copypaste from PlusExpr, but what the hell...
        def des(it: (String, spl.UnaryExpr)): (String, ast.Expr) =
            (it._1 , desugar(it._2))
        def cons(left: (String, ast.Expr),
                right: (String, ast.Expr)): (String, ast.Expr) =
            (right._1,
            ast.Binary(ast.BinaryOp.withName(right._1),
                    left._2,
                    right._2))

        val lst = ("", expr.left) :: expr.op.map(_.text).zip(expr.rest)
        val ret = splitLeftAssoc(lst, cons)(des)
        ret._2
    }

    def desugar(expr: spl.UnaryExpr): ast.Expr = expr match {
        case n: spl.NegExpr =>
            ast.Unary(ast.UnaryOp.Neg, desugar(n.expr))
        case n: spl.NotExpr =>
            ast.Unary(ast.UnaryOp.Not, desugar(n.expr))
        case s: spl.SelectExpr =>
            ast.Select(desugar(s.sel), desugar(s.tuple))
        case a: spl.ApplyExpr =>
            if (a.params.isEmpty)
                desugar(a.fun)
            else
                ast.Apply(desugar(a.fun), a.params.map(desugar))
    }

    def desugar(expr: spl.PrimaryExpr): ast.Expr = expr match {
        case i: spl.Id =>
            desugar(i)
        case n: spl.Num =>
            desugar(n)
        case tl: spl.TupleLiteral =>
            if (tl.first eq null)
                ast.TupleLit(Nil)
            else if (tl.rest.isEmpty)
                desugar(tl.first) // expression in parentheses
            else
                ast.TupleLit((tl.first :: tl.rest).map(desugar))
        case sl: spl.ListLiteral =>
            // items in the list literal.
            val elements =
                if (sl.first eq null)
                    Nil
                else
                    (sl.first :: sl.rest).map(desugar)
            elements.foldRight[ast.Expr](ast.ListNil())(
                ast.Binary(ast.BinaryOp.Cons, _, _))

    }

    def desugar(id: spl.Id): ast.Id =
        ast.Id(id.text).withSource(id)

    def desugar(num: spl.Num): ast.Num =
        ast.Num(num.text.toInt)

    implicit def binaryCons(op: ast.BinaryOp.Type):
            (ast.Expr, ast.Expr) => ast.Expr =
        ast.Binary(op, _, _)

    def splitLeftAssoc[S,T](lst: List[S], cons: (T, T) => T)
            (implicit ds: (S) => T): T = {
        val converted = lst.map(ds)
        converted match {
            case h :: t =>
                t.foldLeft(h)(cons)
            case Nil =>
                throw new DesugarException(lst)
        }
    }

    class DesugarException(node: Object)
            extends RuntimeException("Invalid node: " + node) {
    }
}