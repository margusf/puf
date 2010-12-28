package puf

import mama._

import collection.mutable.ArrayBuffer
import java.util.IdentityHashMap

object Reorder {
    def reorder(code: Iterable[Opcode]): Iterable[Opcode] = {
        val obj = new Reorder(code)
        obj.makeBlocks()
//        println("block count: " + obj.blocks.values.size)
        println("Blocks:\n" + obj.blocks.values + "\n\n")
        obj.reorder()
        obj.makeCode()
    }
}

private class Block(label: Label,
                    body: Iterable[Opcode],
                    end: Opcode) {
    val refs = addRef(end, body.foldRight(Set.empty[Label])(addRef))

    private def addRef(op: Opcode, refs: Set[Label]) =
        if (op.isInstanceOf[WithLabel])
            refs + op.asInstanceOf[WithLabel].label
        else
            refs

    override def toString =
        "\n" + label + "\n" +
        body.mkString("\n") + "\n" + end
}

private class Reorder(code: Iterable[Opcode]) {
    val blocks = new IdentityHashMap[Label, Block]
    val mainLabel = new Label

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
        blocks.put(label, new Block(label, body, end))
    }

    def reorder() {
    }

    def makeCode(): Iterable[Opcode] =
        Nil
}
