package puf

import ast._
import mama._

import collection.mutable.ListBuffer

class Env {
}

class Codegen {
    val code = new ListBuffer[Opcode]

    def codeB(expr: Expr, env: Env, sd: Int) = expr match {
        case Num(n) =>
            code += Loadc(n)
        case _ =>
            throw new Exception("Unsupported expression: " + expr)
    }

    def codeOutput =
        code.mkString("", "\n", "\n")
}