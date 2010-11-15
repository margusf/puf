package puf.mama

abstract class Opcode(code: String, params: Array[Any]) {
    def this(code: String) = 
        this(code, Array[Any]())
    def this(code: String, params: Int*) = 
        this(code, params.asInstanceOf[Array[Any]])

    override def toString = code + " " + params.mkString(" ")
}

case class Halt extends Opcode("HALT")
case class Loadc(param: Int) extends Opcode("LOADC", param)
