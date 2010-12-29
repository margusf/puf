package puf

// Main function for the PUF compiler

import ee.cyber.simplicitas.{GeneratorBase, MainBase}
import java.io.File

class PufGenerator(destDir: String)
        extends GeneratorBase(destDir) {
    def generate(tree: spl.Program, pufFile: String) {
        val desugared = Desugar.desugar(tree)
        val out = Codegen.generate(desugared)
        val outFile = super.writeFile(pufFile.replaceAll(".puf", ".cbn"), out)
        println("Output written to " + outFile)
    }
}

/** Main entry point of the compiler. */
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

        // Process include directives.
        if (grammar.tree.include ne null) {
            val includePath = new File(file).getParentFile.getAbsolutePath
            val includeFile = includePath + "/" + grammar.tree.include.file.text
            val included = parseFile(includeFile + ".puf")
            grammar.tree.decls = included.decls ++ grammar.tree.decls
        }

        grammar.tree
    }
}
