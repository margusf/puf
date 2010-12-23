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

        // Tests comparison operators.
        generate("a = 1 > 2; b = 1 < 2; c = 1 <= 2; d = 1 >= 2; main = 10;")

        // List literals and list operations.
        generate("""
            a = [];
            b = 44 : a;
            c = [1, 2, b, 4];
            len l = case l of
                [] -> 0;
                h : t -> 1 + len t;
            main = len c;
            """)

        // A more complicated list example.
        generate("""
            foldl f init l =
                case l of
                    [] -> init;
                    h : t -> f h (foldl f init t);
            map f l =
                case l of
                    [] -> [];
                    h : t -> (f h) : (map f t);
            double = map (fn x -> 2 * x);
            sum = foldl (fn x y -> x + y) 0;
            compose f g = fn x -> f (g x);
            sumDoubles = compose sum double;
            main = sumDoubles [1, 2, 3, 4, 5];
            """)

        // Tuple literals, selection operator and tuple support in let.
        generate("""
            x = (1, 2, 3);
            y = (x, x);
            main = let
                foo = #2 (#1 y);
                (a, b, bar) = x;
                in
                    if foo == bar
                    then 1
                    else 0;
            """)

        // Dependencies in letrec expressions
        generate("""
            d = a + (fn x -> x + c) 10;
            c = letrec
                x = a;
            in
                x + b;
            f = e c;
            main = f + c;
            e x = a + b + x;
            a = 10;
            b = 20;
            """)
        try {
            generate("""
                a = 10;
                b = c + a;
                c = letrec
                    x = y + 10;
                    y = 20;
                in
                    x * y + b;
                """)
            throw new Error("Failed to detect error")
        } catch {
            case e: Exception =>
                println("Caught expected error: " + e.getMessage)
        }
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
