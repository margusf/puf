package puf

import ast._

import ee.cyber.simplicitas.GeneratorBase

object TestCodegen extends GeneratorBase(null) {
    def main(args: Array[String]) {
        generate(Num(666))
        generate(Unary(UnaryOp.Neg, Num(666)))
        generate(Binary(
            BinaryOp.Plus,
            Binary(
                BinaryOp.Minus,
                Binary(
                    BinaryOp.Minus,
                    Binary(
                        BinaryOp.Plus,
                        Binary(
                            BinaryOp.Plus,
                            Num(1),
                            Num(2)),
                        Num(3)),
                    Num(4)),
                Num(5)),
            Num(7)))
    }

    var count = 1

    def generate(expr: Expr) {
        println("Code for: " + expr)

        val filename = "target/tests/test" + count + ".cbn"
        count += 1
        val codegen = new Codegen
        codegen.codeB(expr, new Env, 0)
        codegen.code += mama.Halt()
        println("Saved as: " + filename)
        println(codegen.codeOutput)
        writeFile(filename, codegen.codeOutput)
    }
}