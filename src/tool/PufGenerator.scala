package puf

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class PufGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("Puf.stg")
    
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
    val grammar = new spl.PufGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)

      new PufGenerator(destDir).generate(grammar.tree)        
    }
  }
}
