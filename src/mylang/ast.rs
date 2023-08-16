use crate::mylang::preproc::*;
use crate::mylang::SourceLoader;
use std::mem::discriminant;

#[derive(Clone, Debug)]
pub enum Item {
    Func(FuncDef),
    Const(ConstDef),
    Global(GlobalDef),
}
#[derive(Clone, Debug)]
pub enum Type {
    I16(Span),
    U16(Span),
    Ptr(Box<Type>),
    U8(Span),
    I8(Span),
    Bool(Span),
}

impl Type {
    pub fn type_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ptr(a), Self::Ptr(b)) => a.type_eq(b),
            _ => discriminant(self) == discriminant(&other),
        }
    }
    pub fn span(&self) -> Span {
        use Type::*;
        match self {
            I16(v) | U16(v) | I8(v) | U8(v) | Bool(v) => *v,
            Self::Ptr(v) => v.span(),
        }
    }
    pub fn size_of(&self) -> u32 {
        use Type::*;
        match self {
            I16(_) | U16(_) | Ptr(_) => 2,
            I8(_) | U8(_) | Bool(_) => 1,
        }
    }
    pub fn signed(&self) -> bool {
        use Type::*;
        match self {
            Bool(_) | U16(_) | Ptr(_) | U8(_) => false,
            I8(_) | I16(_) => true,
        }
    }
}
#[derive(Clone, Debug)]
pub struct Argument {
    pub name: Identifier,
    pub ty: Type,
}
#[derive(Clone, Debug)]
pub struct Identifier {
    pub span: Span,
    pub value: String,
}
#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: Identifier,
    pub inline: bool,
    pub ret: Option<Type>,
    pub args: Vec<Argument>,
    pub stt: Vec<Statement>,
}
#[derive(Clone, Debug)]
pub struct ConstDef {
    pub name: Identifier,
    pub ty: Type,
    pub init: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct GlobalDef {
    //globals cannot have initializers, their value is undefined at the start of program
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Index(Box<Expression>, Box<Expression>),  // expr1[expr2]
    Pointer(Box<Expression>),                 // *expr
    Call(Identifier, Vec<Expression>),        // name(expr0, expr1, ... , exprN)
    Add(Box<Expression>, Box<Expression>),    // expr1 + expr2
    Sub(Box<Expression>, Box<Expression>),    // expr1 - expr2
    Mul(Box<Expression>, Box<Expression>),    // expr1 * expr2
    Shl(Box<Expression>, Box<Expression>),    // expr1 << expr2
    Shr(Box<Expression>, Box<Expression>),    // expr1 >> expr2
    AriShr(Box<Expression>, Box<Expression>), // expr1 >>> expr2 `arithmetic shift`
    Gr(Box<Expression>, Box<Expression>),     // expr1 > expr2
    Ge(Box<Expression>, Box<Expression>),     // expr1 >= expr2
    Lo(Box<Expression>, Box<Expression>),     // expr1 < expr2
    Le(Box<Expression>, Box<Expression>),     // expr1 <= expr2
    Eq(Box<Expression>, Box<Expression>),     // expr1 == expr2
    Ne(Box<Expression>, Box<Expression>),     // expr1 != expr2
    And(Box<Expression>, Box<Expression>),    // expr1 & expr2
    Or(Box<Expression>, Box<Expression>),     // expr1 | expr2
    Xor(Box<Expression>, Box<Expression>),    // expr1 ^ expr2
    Number(i64, bool, Span),                  // number literal, bool is signed/unsigned
    Cast(Type, Box<Expression>),              // expr as ty
    Neg(Box<Expression>),                     // -expr
    Not(Box<Expression>),                     // !expr
    Name(Identifier),                         // identifier
    Paren(Box<Expression>),                   // ( expr )
    ArrayInit(Span, Vec<Expression>),         // [ expr0 , expr1 , ... , exprN ]
}
impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Self::Index(a, b) | Self::Add(a, b) | Self::Mul(a, b) => a.span().merge(b.span()),
            Self::Shr(a, b) | Self::AriShr(a, b) | Self::Shl(a, b) | Self::Sub(a, b) => a.span().merge(b.span()),
            Self::Number(_, _, s) => *s,
            Self::Cast(t, e) => e.span().merge(t.span()),
            Self::Neg(v) | Self::Not(v) | Self::Paren(v) | Self::Pointer(v) => v.span(),
            Self::Name(v) => v.span,
            Self::ArrayInit(v, _) => *v,
            Self::Call(v, _) => v.span,
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct AstError {}

#[derive(Clone, Debug)]
pub enum Statement {
    Var { name: Identifier, ty: Type, expr: Box<Expression> }, // var name: ty = expr;
    While { cond: Box<Expression>, block: Vec<Statement> },    // while cond { ... }
    DoWhile { block: Vec<Statement>, cond: Box<Expression> },  // do { ... } while cond;
    LoopForever(Vec<Statement>),                               // loop { ... }
    Return(Span, Option<Box<Expression>>),                     // return expr;
    Assign(Box<Expression>, Box<Expression>),                  // expr1 = expr2;
    If { cond: Box<Expression>, block: Vec<Statement>, els: Option<Vec<Statement>> }, // if cond { block.. } else { els.. }
    Break(Span),                                                                      // break;
    Continue(Span),                                                                   // continue;
    Expr(Box<Expression>),                                                            // expr;
    Block(Vec<Statement>),                                                            // { block.. }
}

peg::parser!(pub grammar parser() for str {


    // rule identifier(ann: &LineAnnotation) -> Identifier
    //     = quiet!{ n:$(ident_start() ident_part()*) { Identifier{v:ann.spanned(n)} }}
    //     / expected!("identifier")
    // // helpers *******************************************************
    // rule any_not_paren(ann: &LineAnnotation)->TextSpan
    //     = [_][_] { TextSpan::default() }
    rule ident_start() = ch:$([_]) {? if is_label_start(ch.chars().next().unwrap()) {Ok(())} else {Err("Identifier start")}}
    rule ident_part() = ch:$([_]) {? if is_label_part(ch.chars().next().unwrap()) {Ok(())} else {Err("Identifier part")}}
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

pub fn parse_ast(main_file: &str, loader: Box<dyn SourceLoader>) -> (Result<Vec<Item>, Vec<AstError>>, SourceMap) {
    todo!()
}
