package puf.ast

import ee.cyber.simplicitas.SourceLocation

trait WithSource extends SourceLocation {
    var loc: SourceLocation = null

    def startIndex = loc.startIndex
    def endIndex = loc.endIndex
    def startLine = loc.startLine
    def endLine = loc.endLine
    def startColumn = loc.startColumn
    def endColumn = loc.endColumn

    def withSource(src: SourceLocation): this.type = {
        loc = src
        this
    }
}

// The actual AST classes, desugared from the ugly Simpl AST.
trait Expr extends WithSource

case class Case(cond: Expr, nilAlt: Expr, consAlt: ConsAlt) extends Expr
case class ConsAlt(head: Id, tail: Id, expr: Expr) extends WithSource
case class Letrec(decls: List[Decl], expr: Expr) extends Expr
case class Let(decls: List[Decl], expr: Expr) extends Expr
case class Decl(left: DeclLeft, right: Expr) extends WithSource
trait DeclLeft extends WithSource
case class TupleLeft(items: List[Id]) extends DeclLeft
case class Lambda(params: List[Id], expr: Expr) extends Expr
case class If(cond: Expr, ifExpr: Expr, elseExpr: Expr) extends Expr
case class Binary(op: BinaryOp.Type, left: Expr, right: Expr) extends Expr
case class Unary(op: UnaryOp.Type, expr: Expr) extends Expr
case class Apply(fun: Expr, params: List[Expr]) extends Expr
case class Select(sel: Num, expr: Expr) extends Expr
case class TupleLit(parts: List[Expr]) extends Expr
case class ListNil extends Expr
case class Cons(left: Expr, right: Expr) extends Expr

case class Id(text: String) extends Expr with DeclLeft
case class Num(num: Int) extends Expr

object BinaryOp extends Enumeration {
    type Type = Value

    val Plus = Value("+")
    val Minus = Value("-")
    val Times = Value("*")
    val Div = Value("/")
    val Mod = Value("%")
    val LessThan = Value("<")
    val LessEqual = Value("<=")
    val GreaterThan = Value(">")
    val GreaterEqual = Value(">=")
    val Equals = Value("==")
    val NotEquals = Value("/=")
    val And = Value("&&")
    val Or = Value("||")
}

object UnaryOp extends Enumeration {
    type Type = Value

    val Neg, Not = Value
}
