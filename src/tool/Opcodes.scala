package puf.mama

abstract class Opcode(code: String, params: Array[Any]) {
    def this(code: String) = 
        this(code, Array[Any]())
    def this(code: String, param: Int) = 
        this(code, Array[Any](param))

    override def toString = code + " " + params.mkString(" ")
}

case class Halt extends Opcode("HALT")
case class Loadc(param: Int) extends Opcode("LOADC", param)
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
