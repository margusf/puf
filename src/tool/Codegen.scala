package puf

import ast._
import mama._

import collection.mutable.ListBuffer

class Codegen {
    import Codegen._
    import LetrecHelper._

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
            case Id(text) =>
                getVar(text, env, sd)
            case ListNil() =>
                code += NilOp()
            case Cons(l, r) =>
                codeV(l, env, sd)
                codeV(r, env, sd + 1)
                code += ConsOp()
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
            case Let(decls, expr) =>
                val (newEnv, newSd) =
                    decls.foldLeft((env, sd))(processDecl)
                codeV(expr, newEnv, newSd)
                code += Slide(decls.size)
            case Letrec(decls, expr) =>
                val newDecls = checkLetrecDecls(decls)
                val mappings = letrecMappings(newDecls, sd)
                val newEnv = env.extend(mappings)
                println("letrec, newenv = " + newEnv)
                val newSd = sd + mappings.size
                var rewrite = mappings.size

                code += Alloc(mappings.size)

                for (decl <- decls) {
                    codeV(decl.right, newEnv, newSd)
                    code += Rewrite(rewrite)
                    rewrite -= 1
                }

                codeV(expr, newEnv, newSd)
                code += Slide(mappings.size)
            case Lambda(params, body) =>
                val freeVars = FreeVars.get(expr, false).toList
                val bodyEnv = Env.functionEnv(freeVars, params.map(_.text))
                println("Bodyenv: " + bodyEnv)
                val bodyLbl = Label()
                val cont = Label()
                var newSd = sd
                for (fv <- freeVars) {
                    getVar(fv, env, newSd)
                    newSd += 1
                }
                code += Mkvec(freeVars.size)
                code += Mkfunval(bodyLbl)
                code += Jump(cont)
                code += bodyLbl
                code += Targ(params.size)
                codeV(body, bodyEnv, 0)
                code += Return(params.size)
                code += cont
            case Apply(fun, params) =>
                val cont = Label()
                code += Mark(cont)
                var newSd = sd + 3
                for (param <- params.reverse) {
                    codeV(param, env, newSd)
                    newSd += 1
                }
                codeV(fun, env, newSd)
                code += ApplyOp()
                code += cont
            case Case(cond, nilExpr, ConsAlt(h, t, consExpr)) =>
                val listLbl = Label()
                val cont = Label()
                val newEnv = env.extend(
                    Map(h.text -> (sd + 1), t.text -> (sd + 2)))

                codeV(cond, env, sd)
                code += Tlist(listLbl)
                codeV(nilExpr, env, sd)
                code += Jump(cont)
                code += listLbl
                codeV(consExpr, newEnv, sd + 2)
                code += Slide(2)
                code += cont
            case _ => throw new Exception("Unsupported expression: " + expr)
        }
    }

    def getVar(id: String, env: Env, sd: Int) {
        val (vType, i) = env(id)
        vType match {
            case VarType.Local =>
                code += Pushloc(sd - i)
            case VarType.Global =>
                code += Pushglob(i)
        }
    }

    /** Helper function for generating code for declaration items in let
      * expression. */
    def processDecl(prev: Tuple2[Env, Int], decl: Decl):
            Tuple2[Env, Int] = {
        val (env, sd) = prev
        codeV(decl.right, env, sd)
        val newSd = sd + 1
        decl.left match {
            case Id(txt) =>
                (env.extend(Map(txt -> newSd)), newSd)
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

object Codegen {
    def generate(expr: Expr) = {
        val gen = new Codegen()
        gen.codeV(expr, Env.empty, 0)
        gen.code += mama.Halt()
        gen.finalizeCode()
        gen.codeOutput
    }

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
            BinaryOp.GreaterThan -> Gr(),
            BinaryOp.GreaterEqual -> Geq(),
            BinaryOp.And -> And(),
            BinaryOp.Or -> Or(),
            BinaryOp.Equals -> Eq(),
            BinaryOp.NotEquals -> Neq())
}