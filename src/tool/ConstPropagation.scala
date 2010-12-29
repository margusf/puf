package puf

import ast._

object ConstPropagation {
    type Val = Option[Int]

    class Env(contents: Map[String, Val]) {
        def apply(varName: String) =
            contents.get(varName) match {
                case None => None
                case Some(None) => None
                case Some(Some(x)) => Some(x)
            }

        def extend(bindings: List[Tuple2[String, Val]]) =
            new Env(contents ++ bindings.toMap)
        def extend(id: String, value: Val) =
            new Env(contents ++ Map(id -> value))
        override def toString = contents.toString
    }

    object Env {
        val empty = new Env(Map.empty)
    }

    def optimize(expr: Expr): Expr =
        optimize(expr, Env.empty)

    def optimize(expr: Expr, env: Env): Expr = expr match {
        case Unary(op, arg) =>
            val optArg = optimize(arg, env)
            (op, optArg) match {
                case (UnaryOp.Neg, Num(x)) => Num(-x)
                case (UnaryOp.Not, Bool(x)) => Bool(!x)
                case _ => Unary(op, optArg)
            }
        case  Binary(op, arg1, arg2) =>
            optimizeBinary(op, arg1, arg2, env)
        case Id(text) =>
            env(text) match {
                case None => Id(text)
                case Some(x) => Num(x)
            }
        case Cons(l, r) =>
            Cons(optimize(l, env), optimize(r, env))
        case If(cond, ifExpr, elseExpr) =>
            val optCond = optimize(cond, env)
            optCond match {
                case Bool(b) =>
                    if (b)
                        optimize(ifExpr, env)
                    else
                        optimize(elseExpr, env)
                case _ => If(optCond,
                    optimize(ifExpr, env),
                    optimize(elseExpr, env))
            }
        case Let(decls, expr) =>
            optimizeLet(decls, expr, env)
        case Letrec(decls, expr) =>
            val bindings = decls.map(
                (d: Decl) => d.left match {
                    case Id(id) => (id, None)
                })
            val newEnv = env.extend(bindings)

            Letrec(
                decls.map(
                    (d: Decl) =>
                        Decl(d.left, optimize(d.right, newEnv))),
                optimize(expr, newEnv))
        case Lambda(params, body) =>
            val bindings = params.map(
                (id: Id) => (id.text, None))
            Lambda(params, optimize(body, env.extend(bindings)))
        case Apply(fun, params) =>
            Apply(optimize(fun, env),
                params.map(optimize(_, env)))
        case Case(cond, nilExpr, ConsAlt(Id(h), Id(t), consExpr)) =>
            val optCons = optimize(consExpr,
                env.extend(List(h -> None, t -> None)))
            Case(optimize(cond, env),
                    optimize(nilExpr, env),
                    ConsAlt(Id(h), Id(t), optCons))
        case TupleLit(items) =>
            TupleLit(items.map(optimize(_, env)))
        case Select(idx, expr) =>
            Select(idx, optimize(expr, env))
        case _ =>
            expr
    }

    private def optimizeLet(decls: List[Decl], expr: Expr, env: Env): Expr = {
        def loop(todo: List[Decl], done: List[Decl], newEnv: Env): Expr =
            todo match {
                case Nil =>
                    val optExpr = optimize(expr, newEnv)
                    optExpr match {
                        case Num(x) => Num(x)
                        case x => Let(done, x)
                    }
                case Decl(Id(id), right) :: rest =>
                    val optRight = optimize(right, newEnv)
                    optRight match {
                        case Num(x) =>
                            loop(rest, done, newEnv.extend(id, Some(x)))
                        case _ =>
                            loop(rest, done ++ List(Decl(Id(id), optRight)),
                                    newEnv.extend(id, None))
                    }
                case Decl(TupleLeft(ids), right) :: rest =>
                    val optRight = optimize(right, newEnv)
                    val bindings = ids.map(
                        (x: Id) => (x.text, None))

                    loop(rest, done ++ List(Decl(TupleLeft(ids), optRight)),
                            newEnv.extend(bindings))
            }

        loop(decls, Nil, env)
    }

    private def optimizeBinary(op: BinaryOp.Type,
                               arg1: Expr, arg2: Expr, env: Env): Expr = {
        val optArg1 = optimize(arg1, env)
        val optArg2 = optimize(arg2, env)

        (optArg1, optArg2) match {
            case (Num(x1), Num(x2)) if arithmeticOp(op) =>
                arithmOps(op)(x1, x2)
            case (Bool(b1), Bool(b2)) if !arithmeticOp(op) =>
                Bool(booleanOps(op)(b1, b2))
            case (x, Num(0))
                    if ((op eq BinaryOp.Plus) || (op eq BinaryOp.Plus)) =>
                x
            case (Num(0), x)
                    if ((op eq BinaryOp.Plus) || (op eq BinaryOp.Plus)) =>
                x
            case (x, Num(1))
                    if ((op eq BinaryOp.Times) || (op eq BinaryOp.Div)) =>
                x
            case (Num(1), x)
                    if ((op eq BinaryOp.Times) || (op eq BinaryOp.Div)) =>
                x
            case (x, Num(0)) if ((op eq BinaryOp.Times)) =>
                Num(0)
            case (Num(0), x)
                    if ((op eq BinaryOp.Times) || (op eq BinaryOp.Div)) =>
                Num(0)
            case _ =>
                Binary(op, optArg1, optArg2)
        }
    }

    private def arithmeticOp(op: BinaryOp.Type) = op match {
        case BinaryOp.Plus
                | BinaryOp.Minus
                | BinaryOp.Times
                | BinaryOp.Div
                | BinaryOp.Mod
                | BinaryOp.LessThan
                | BinaryOp.LessEqual
                | BinaryOp.GreaterThan
                | BinaryOp.GreaterEqual
                | BinaryOp.Equals
                | BinaryOp.NotEquals => true
        case BinaryOp.And
                | BinaryOp.Or => false
    }

    private val booleanOps = Map(
        BinaryOp.And -> ((x1: Boolean, x2: Boolean) => x1 && x2),
        BinaryOp.Or -> ((x1: Boolean, x2: Boolean) => x1 || x2)
    )

    private val arithmOps = Map[BinaryOp.Type, Function2[Int, Int, Expr]](
        BinaryOp.Plus -> ((x: Int, y: Int) => Num(x + y)),
        BinaryOp.Minus -> ((x: Int, y: Int) => Num(x - y)),
        BinaryOp.Times -> ((x: Int, y: Int) => Num(x * y)),
        BinaryOp.Div -> ((x: Int, y: Int) => Num(x / y)),
        BinaryOp.Mod -> ((x: Int, y: Int) => Num(x % y)),
        BinaryOp.LessThan -> ((x: Int, y: Int) => Bool(x < y)),
        BinaryOp.LessEqual -> ((x: Int, y: Int) => Bool(x <= y)),
        BinaryOp.GreaterThan -> ((x: Int, y: Int) => Bool(x > y)),
        BinaryOp.GreaterEqual -> ((x: Int, y: Int) => Bool(x >= y)),
        BinaryOp.Equals -> ((x: Int, y: Int) => Bool(x == y)),
        BinaryOp.NotEquals -> ((x: Int, y: Int) => Bool(x != y))
    )
}
