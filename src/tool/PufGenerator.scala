package puf

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class PufGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
    def generate(tree: spl.Program) {
        println("Dump")
        println(Dump.dumpNode(tree))

        val desugared = Desugar.desugar(tree)
        println("Desugared:")
        println(desugared)
    }
}

object PufMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        for (arg <- sources) {
            val tree = parseFile(arg)
            new PufGenerator(destDir).generate(tree)        
        }
    }
    
    def parseFile(file: String): spl.Program = {
        val grammar = new spl.PufGrammar()
        grammar.parseFile(file)
        checkErrors(grammar.errors)

        if (grammar.tree.include ne null) {
            val included = parseFile(grammar.tree.include.file.text + ".puf")
            grammar.tree.decls = included.decls ++ grammar.tree.decls
        }

        grammar.tree
    }
}
