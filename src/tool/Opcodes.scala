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
case class Loadc(param: Int) extends Opcode("LOADC", param)
case class Getbasic extends Opcode("GETBASIC")
case class Mkbasic extends Opcode("MKBASIC")

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
case class Ge extends Opcode("GE")
case class Geq extends Opcode("GEQ")
case class Eq extends Opcode("EQ")
case class Neq extends Opcode("NEQ")
case class And extends Opcode("AND")
case class Or extends Opcode("OR")
