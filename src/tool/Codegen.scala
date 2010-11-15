package puf

import ast._
import mama._

import collection.mutable.ListBuffer

class Env {
}

object Codegen {
    val unaryOps = Map(
            UnaryOp.Not -> Not(),
            UnaryOp.Neg -> Neg())

    val binaryOps = Map(
            BinaryOp.Plus -> Add(),
            BinaryOp.Minus -> Sub(),
            BinaryOp.Times -> Mul(),
            BinaryOp.Div -> Div(),
            BinaryOp.Mod -> Mod(),
            BinaryOp.LessThan -> Le(),
            BinaryOp.LessEqual -> Leq(),
            BinaryOp.GreaterThan -> Ge(),
            BinaryOp.GreaterEqual -> Geq(),
            BinaryOp.And -> And(),
            BinaryOp.Or -> Or(),
            BinaryOp.Equals -> Eq(),
            BinaryOp.NotEquals -> Neq())
}

class Codegen {
    import Codegen._

    val code = new ListBuffer[Opcode]

    def codeB(expr: Expr, env: Env, sd: Int) {
        expr match {
            case Num(n) =>
                code += Loadc(n)
            case Unary(op, e) =>
                codeB(e, env, sd)
                code += unaryOps(op)
            case Binary(op, l, r) =>
                codeB(l, env, sd)
                codeB(r, env, sd + 1)
                code += binaryOps(op)
            case If(cond, ifExpr, elseExpr) =>
                // Labels for else and for code after if block.
                val lblElse = Label()
                val lblCont = Label()

                codeB(cond, env, sd)
                code += Jumpz(lblElse)
                codeB(ifExpr, env, sd)
                code += Jump(lblCont)
                code += lblElse
                codeB(elseExpr, env, sd)
                code += lblCont
            case _ =>
                throw new Exception("Unsupported expression: " + expr)
        }
    }

    def finalizeCode() {
        var labelCounter = 0
        
        def nameProvider = {
            labelCounter += 1
            "L" + labelCounter
        }

        for (instr <- code) {
            instr match {
                case l: Label => l.init(nameProvider)
                case _ => ()
            }
        }
    }
    
    def codeOutput =
        code.mkString("", "\n", "\n")
}