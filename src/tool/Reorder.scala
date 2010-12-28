package puf

import mama._

import collection.mutable.ArrayBuffer

object Reorder {
    def reorder(code: Iterable[Opcode]): Iterable[Opcode] = {
        val obj = new Reorder(code)
        obj.makeBlocks()
//        println("block count: " + obj.blocks.values.size)
        println("Blocks:\n" + obj.blocks.values + "\n\n")
        obj.mergeBlocks()
        obj.makeCode()
    }
}

private class Block(label: Label,
                    body: Iterable[Opcode],
                    end: Opcode) {
    def name = label.name

    override def toString =
        "\n" + label + "\n" +
        body.mkString("\n") + "\n" + end
}

private class Reorder(code: Iterable[Opcode]) {
    val blocks = collection.mutable.Map[String, Block]()
    val refs =
        new collection.mutable.HashMap[String, collection.mutable.Set[String]]
        with collection.mutable.MultiMap[String, String]
    val mainLabel = new Label
    mainLabel.name = "TOPLEVEL"

    def makeBlocks() {
        var label = mainLabel
        var body = new ArrayBuffer[Opcode]

        for (op <- code) {
            if (op.isInstanceOf[Label]) {
                makeBlock(label, body)
                label = op.asInstanceOf[Label]
                body = new ArrayBuffer[Opcode]
            } else {
                body += op
            }
        }

        makeBlock(label, body)
    }

    def isBlockEnd(op: Opcode) = op match {
        case Jump(_)
            | Halt()
            | ApplyOp()
            | Return(_) => true
        case _ => false
    }

    def makeBlock(label: Label, body: ArrayBuffer[Opcode]) {
        val end = body(body.length - 1)
        if (!isBlockEnd(end)) {
            throw new Exception(
                "Block does not end with terminating opcode, but " + end)
        }
        body.remove(body.length - 1)
        blocks(label.name) = new Block(label, body, end)

        addRefs(label.name, end :: body.toList)
    }

    private def addRefs(name: String, ops: Iterable[Opcode]) {
        for (op <- ops) {
            if (op.isInstanceOf[WithLabel]) {
                val label = ops.asInstanceOf[WithLabel].label
                refs.addBinding(label.name, name)
            }
        }
    }

    def mergeBlocks() {
        for (block <- blocks.values.toSet[Block]) {
            val blockRefs = refs.getOrElse(block.name,
                collection.mutable.Set.empty)
            // Is there only one reference to this block?
            if (blockRefs.length == 1) {
            }
        }
    }

    def makeCode(): Iterable[Opcode] =
        Nil
}
