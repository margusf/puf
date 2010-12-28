package puf

import mama._

import collection.mutable.ArrayBuffer

object Reorder {
    def reorder(code: Iterable[Opcode]): Iterable[Opcode] = {
        val obj = new Reorder(code)
        obj.separate()
        obj.reassembledCode
    }
}

private class Reorder(input: Iterable[Opcode]) {
    type Block = ArrayBuffer[Opcode]

    val blocks = new ArrayBuffer[Block]
    var topLevel: Block = null

    def separate() {
        makeBlocks(input.toList, null,
                (block: Block) => topLevel = block)
    }

    def reassembledCode =
        topLevel ++ blocks.flatten

    def makeBlocks(code: List[Opcode], endLabel: Label,
                   storeFun: (Block) => Unit): List[Opcode] = {
        val block = new Block

        def loop(lst: List[Opcode]): List[Opcode] = lst match {
            case lbl :: rest if lbl eq endLabel =>
                // We have reached the end of the block
                // Remove the label since it is not used anymore.
                rest
            case Mkfunval(fLbl) :: Jump(cont) :: rest =>
                // This is code of the function. Put it into a block
                val contCode = makeBlocks(rest, cont, storeBlock)

                // Keep the MKFUNVAL, but skip the jump that will not
                // be necessary any more.
                block += Mkfunval(fLbl)

                loop(contCode)
            case op :: rest =>
                // Just another opcode in the wall
                block += op
                loop(rest)
            case Nil =>
                // In case the endLabel was null
                Nil
        }

        val ret = loop(code)
        storeFun(block)
        ret
    }

    def storeBlock(block: Block) {
        blocks += block
    }
}