package puf

import ast._
import mama._

import collection.mutable.ListBuffer

class Env(val parent: Env, mapping: Map[String, Int]) {
    def apply(id: String): Int =
        if (mapping.contains(id))
            mapping(id)
        else
            parent(id)

    def extend(mapping: Map[String, Int]) =
        new Env(this, mapping)
}

object Env {
    val empty = new Env(null, null) {
        override def apply(id: String): Int =
            throw new Exception("Unknown identifier: " + id)
    }
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
                codeV(expr, env, sd)
                code += Getbasic()
        }
    }

    def codeV(expr: Expr, env: Env, sd: Int) {
        expr match {
            // Primitives are done with codeB
            case Num(_) | Unary(_, _) | Binary(_, _, _) =>
                codeB(expr, env, sd)
                code += Mkbasic()
            // If is very similar to codeB, but we use codeV to
            // generate then and else branches and so avoid
            // unnecessary MKBASIC operation.
            case If(cond, ifExpr, elseExpr) =>
                // Labels for else and for code after if block.
                val lblElse = Label()
                val lblCont = Label()

                codeB(cond, env, sd)
                code += Jumpz(lblElse)
                codeV(ifExpr, env, sd)
                code += Jump(lblCont)
                code += lblElse
                codeV(elseExpr, env, sd)
                code += lblCont
            case Id(text) =>
                val i = env(text)
                code += Pushloc(sd - i)
            case Let(decls, expr) =>
                val (newEnv, newSd) = 
                    decls.foldLeft((env, sd))(processDecl)
                codeV(expr, newEnv, newSd)
                code += Slide(decls.size)
            case _ => throw new Exception("Unsupported expression: " + expr)
        }
    }

    /** Helper function for generating code for declaration items in let
      * expression. */
    def processDecl(prev: Tuple2[Env, Int], decl: Decl): 
            Tuple2[Env, Int] = {
        val (env, sd) = prev
        codeV(decl.right, env, sd)
        decl.left match {
            case Id(txt) =>
                (env.extend(Map(txt -> (sd + 1))), sd + 1)
            case TupleLeft(items) =>
                // TODO: add support for tuples.
                throw new Exception("Unsupported: tuple let")
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