package puf

import ast._

object TestCodegen {
    def main(args: Array[String]) {
        generate(Num(666))
        generate(Unary(UnaryOp.Neg, Num(666)))
    }

    def generate(expr: Expr) {
        println("Code for: " + expr)

        val codegen = new Codegen
        codegen.codeB(expr, new Env, 0)
        println()
        println(codegen.codeOutput)
    }
}