package puf

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class PufGenerator(destDir: String)
        extends GeneratorBase(destDir) {
    def generate(tree: spl.Program, pufFile: String) {
        val desugared = Desugar.desugar(tree)
        TailCall.markCalls(desugared)
        val out = Codegen.generate(desugared)
        val outFile = super.writeFile(pufFile.replaceAll(".puf", ".cbn"), out)
        println("Output written to " + outFile)
    }
}

object PufMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        for (arg <- sources) {
            val tree = parseFile(arg)
            new PufGenerator(destDir).generate(tree, arg)
        }
    }

    def parseFile(file: String): spl.Program = {
        val grammar = new spl.PufGrammar()
        grammar.parseFile(file)
        checkErrors(grammar.errors)

        if (grammar.tree.include ne null) {
            // TODO: search the dir of the original file.
            val included = parseFile(grammar.tree.include.file.text + ".puf")
            grammar.tree.decls = included.decls ++ grammar.tree.decls
        }

        grammar.tree
    }
}
