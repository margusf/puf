package puf

import ast._

import ee.cyber.simplicitas.{GeneratorBase, SourceMessage}

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
        generate(If(Num(1), Num(2), Num(3)))
        generate(If(
                Binary(BinaryOp.Minus, Num(1), Num(1)),
                Num(2),
                Num(3)))
        generate("a = 10; b = a + 1; main = let x = a; y = b; in x + y;")
        generate("a = 1; b = 2; f x y = b + x + y; g = f 10; main = g 100;")
        generate("p x = fn y -> x + y; main = p 1 2;")
        generate("even x = if x == 0 then 1 else odd (x - 1);" +
        		"odd x = if x == 0 then 0 else even (x - 1);" +
        		"main = even 10;")
		generate("a = 1 > 2; b = 1 < 2; c = 1 <= 2; d = 1 >= 2; main = 10;")
    }

    var count = 1

    def generate(expr: Expr) {
        println("Code for: " + expr)

        val filename = "target/tests/test" + count + ".cbn"
        count += 1
        val codegen = new Codegen
        codegen.codeV(expr, Env.empty, 0)
        codegen.code += mama.Halt()
        codegen.finalizeCode()
        println("Saved as: " + filename)
        println(codegen.codeOutput)
        writeFile(filename, codegen.codeOutput)
    }
    
    def generate(str: String) {
        println("Code: " + str)
        val grammar = new spl.PufGrammar()
        grammar.parseString(str)
        checkErrors(grammar.errors)
        generate(Desugar.desugar(grammar.tree))
    }

    def checkErrors(errors: Collection[SourceMessage]) {
        if (!errors.isEmpty) {
            Console.err.println("Messages")
            errors foreach (Console.err.println)
            exit(1)
        }
   }
}