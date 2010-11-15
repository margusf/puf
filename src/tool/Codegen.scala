package puf

import ast._
import mama._

import collection.mutable.ListBuffer

class Env {
}

object Codegen {
    val unaryOps = Map(
            UnaryOp.Not -> Not(),
            UnaryOp.Neg -> Neg())
}

class Codegen {
    import Codegen._

    val code = new ListBuffer[Opcode]

    def codeB(expr: Expr, env: Env, sd: Int) {
        expr match {
            case Num(n) =>
                code += Loadc(n)
            case Unary(op, e) =>
                codeB(e, env, sd)
                code += unaryOps(op)
            case _ =>
                throw new Exception("Unsupported expression: " + expr)
        }
    }

    def codeOutput =
        code.mkString("", "\n", "\n")
}