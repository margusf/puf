package puf.mama

abstract class Opcode(code: String, params: Array[Any]) {
    def this(code: String) =
        this(code, Array[Any]())
    def this(code: String, param: Int) =
        this(code, Array[Any](param))
    def this(code: String, param: Label) =
        this(code, Array[Any](param))

    override def toString =
        code + " " + params.map(doParam).mkString(" ")

    def doParam(p: Any): String = p match {
        case l: Label => l.name
        case _ => String.valueOf(p)
    }
}

case class Label extends Opcode("LABEL") {
    var name: String = null

    def init(nameProvider: => String) {
        if (name eq null) {
            name = nameProvider
        }
    }

    override def toString = name + ":"
}

case class Jump(label: Label) extends Opcode("JUMP", label)
case class Jumpz(label: Label) extends Opcode("JUMPZ", label)
case class Halt extends Opcode("HALT")
case class Loadc(loc: Int) extends Opcode("LOADC", loc)
case class Pushloc(loc: Int) extends Opcode("PUSHLOC", loc)
case class Pushglob(loc: Int) extends Opcode("PUSHGLOB", loc)
case class Slide(n: Int) extends Opcode("SLIDE", n)
case class Alloc(n: Int) extends Opcode("ALLOC", n)
case class Rewrite(loc: Int) extends Opcode("REWRITE", loc)
case class Getbasic extends Opcode("GETBASIC")
case class Mkbasic extends Opcode("MKBASIC")
case class Mkvec(size: Int) extends Opcode("MKVEC", size)
case class Get(idx: Int) extends Opcode("GET", idx)
case class Mkfunval(label: Label) extends Opcode("MKFUNVAL", label)
case class Targ(n: Int) extends Opcode("TARG", n)
case class Return(n: Int) extends Opcode("RETURN", n)
case class Mark(label: Label) extends Opcode("MARK", label)
case class ApplyOp extends Opcode("APPLY")

case class NilOp extends Opcode("NIL")
case class ConsOp extends Opcode("CONS")
case class Tlist(consLbl: Label) extends Opcode("TLIST", consLbl)

// Arithmetic operators.
case class Neg extends Opcode("NEG")
case class Add extends Opcode("ADD")
case class Sub extends Opcode("SUB")
case class Mul extends Opcode("MUL")
case class Div extends Opcode("DIV")
case class Mod extends Opcode("MOD")
case class Not extends Opcode("NOT")
case class Le extends Opcode("LE")
case class Leq extends Opcode("LEQ")
case class Gr extends Opcode("GR")
case class Geq extends Opcode("GEQ")
case class Eq extends Opcode("EQ")
case class Neq extends Opcode("NEQ")
case class And extends Opcode("AND")
case class Or extends Opcode("OR")
