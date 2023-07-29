use crate::mylang::preproc::{is_whitespace, Span};

#[derive(Clone, Debug)]
pub enum Item{
    Func(FuncDef),
    Const(ConstDef),
}

enum Type{
    Int(Span),
    Pointer(Span),
    Byte(Span),
    Bool(Span),
    Void,
}

struct Argument{
    name: Identifier,
    ty: Type,
}

struct Identifier{
    value: Span,
}

struct FuncDef{
    name: Identifier,
    inline: bool,
    args: Vec<Argument>,
    stt: Vec<Statement>,
}

struct ConstDef{
    name: Identifier,
    ty: Type,
    init: Box<Expression>,
}

enum Expression{
    AssignVar(Identifier,Box<Expression>), //ident = expr
    AssignIndex(Identifier,Box<Expression>,Box<Expression>), //ident[expr1] = expr2
    AssignPointer(Identifier,Box<Expression>), //*ident = expr
    Call(Identifier, Vec<Expression>), // name(expr0, expr1, ... , exprN)
    Add(Box<Expression>,Box<Expression>), // expr1 + expr2
    Sub(Box<Expression>,Box<Expression>), // expr1 - expr2
    Number(i64,Span), // number literal
    Cast(Type, Box<Expression>), // expr as ty
    Neg(Box<Expression>), //-expr
    Not(Box<Expression>), // !expr
    Index(Box<Expression>,Box<Expression>), // expr1[expr2]
    Paren(Box<Expression>), // ( expr )
    ArrayInit(Vec<Expression>), // [ expr0 , expr1 , ... , exprN ]
}

enum Statement{
    Var{name: Identifier, ty: Type, expr: Box<Expression>}, // var name: ty = expr;
    While{cond: Box<Expression>, block: Vec<Statement>}, // while cond { ... }
    DoWhile{block: Vec<Statement>, cond: Box<Expression>}, // do { ... } while cond;
    Return(Box<Expression>), // return expr;
    If{cond: Box<Expression>, block: Vec<Statement>, els: Option<Vec<Statement>>}, // if cond { block.. } else { els.. }
    Break(Span), // break;
    Continue(Span), // continue;
    Expr(Box<Expression>), // expr;
    Block(Vec<Statement>) // { block.. }
}



peg::parser!(pub grammar parser() for str {


    // rule identifier(ann: &LineAnnotation) -> Identifier
    //     = quiet!{ n:$(ident_start() ident_part()*) { Identifier{v:ann.spanned(n)} }}
    //     / expected!("identifier")
    // // helpers *******************************************************
    // rule any_not_paren(ann: &LineAnnotation)->TextSpan
    //     = [_][_] { TextSpan::default() }
    // rule ident_start() = ch:$([_]) {? if is_ident_start(ch.chars().next().unwrap()) {Ok(())} else {Err("Identifier start")}}
    // rule ident_part() = ch:$([_]) {? if is_ident_part(ch.chars().next().unwrap()) {Ok(())} else {Err("Identifier part")}}
    // ignorable *****************************************************
    rule _() =  quiet!{skip()}
    rule skip() = (whitespace() / new_line() / oneline_comment() / multiline_comment())*
    rule any_non(chars: &[char])->bool
        = v:quiet!{(com_nl() {false} / ch:$([_]) {? if chars.contains(&ch.chars().next().unwrap()) {Err("")} else {Ok(true)} })*}
        { v.into_iter().any(|v|v) }
    rule com_nl() = new_line() / oneline_comment() / multiline_comment()
    rule new_line() = "\r\n" / "\r" / "\n" / "\u{2028}" / "\u{2029}" / "\u{0085}"
    rule oneline_comment() = "//" (!new_line() [_])* new_line()
    rule multiline_comment() = "/*" (!"*/" [_])* "*/"
    rule whitespace() = ch:$([_]) {? if is_whitespace(ch.chars().next().unwrap()) {Ok(())} else {Err("whitespace")}}
});