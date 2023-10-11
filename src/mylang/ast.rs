use crate::mylang::preproc::*;
use crate::mylang::SourceLoader;
use peg::error::ExpectedSet;
use std::collections::HashSet;
use std::mem::discriminant;
use std::ops::{Add, Mul, Shl, Shr, Sub};

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
    Ptr(Span, Box<Type>),
    U8(Span),
    I8(Span),
    Bool(Span),
}

impl Type {
    pub fn type_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ptr(_, a), Self::Ptr(_, b)) => a.type_eq(b),
            _ => discriminant(self) == discriminant(&other),
        }
    }
    pub fn type_eq_ptr(&self, ptr_ty: &Self) -> bool {
        if let Self::Ptr(_, ty) = self {
            return ty.type_eq(ptr_ty);
        }
        false
    }
    pub fn span(&self) -> Span {
        use Type::*;
        match self {
            I16(v) | U16(v) | I8(v) | U8(v) | Bool(v) => *v,
            Self::Ptr(a, v) => a.merge(v.span()),
        }
    }
    pub fn is_word_sized(&self) -> bool {
        self.size_of() == 2
    }
    pub fn is_number(&self) -> bool {
        matches!(self, Self::I16(_) | Self::U16(_) | Self::I8(_) | Self::U8(_))
    }
    pub fn size_of(&self) -> u32 {
        use Type::*;
        match self {
            I16(_) | U16(_) | Ptr(_, _) => 2,
            I8(_) | U8(_) | Bool(_) => 1,
        }
    }
    pub fn signed(&self) -> bool {
        use Type::*;
        match self {
            Bool(_) | U16(_) | Ptr(_, _) | U8(_) => false,
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
    AccessName(Box<Expression>, Identifier),  // expr.ident
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
    String(Span, String),                     // "some string"
    Character(Span, String),                  // 'c'
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

impl Add for Box<Expression> {
    type Output = Box<Expression>;
    fn add(self, rhs: Self) -> Self::Output {
        Box::new(Expression::Add(self, rhs))
    }
}
impl Sub for Box<Expression> {
    type Output = Box<Expression>;
    fn sub(self, rhs: Self) -> Self::Output {
        Box::new(Expression::Add(self, rhs))
    }
}
impl Mul for Box<Expression> {
    type Output = Box<Expression>;
    fn mul(self, rhs: Self) -> Self::Output {
        Box::new(Expression::Mul(self, rhs))
    }
}
impl Shr for Box<Expression> {
    type Output = Box<Expression>;
    fn shr(self, rhs: Self) -> Self::Output {
        Box::new(Expression::Shr(self, rhs))
    }
}
impl Shl for Box<Expression> {
    type Output = Box<Expression>;
    fn shl(self, rhs: Self) -> Self::Output {
        Box::new(Expression::Shl(self, rhs))
    }
}

#[derive(Debug)]
pub struct AstError {
    span: Span,
    expected: HashSet<&'static str>,
}

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

#[derive(Debug, Clone)]
pub(crate) enum ItemOrImport {
    Item(Item),
    Import(Vec<Identifier>),
}

peg::parser!(grammar parser() for str {
    pub(crate) rule top(ann: &TextInfo) -> Vec<ItemOrImport>
        = _ it:(item(ann)*) _ { it }

    // ************* Items
    rule item(ann: &TextInfo) -> ItemOrImport
        = _ i:const_def(ann) _ { ItemOrImport::Item(Item::Const(i)) }
        / _ i:global_def(ann) _ { ItemOrImport::Item(Item::Global(i)) }
        / _ i:function_def(ann) _ { ItemOrImport::Item(Item::Func(i)) }
        / _ i:import_def(ann) _ { ItemOrImport::Import(i) }
    rule const_def(ann: &TextInfo) -> ConstDef
        = "const" b:binding(ann) "=" e:expression(ann) ";" { ConstDef{ name: b.0, ty: b.1, init: Box::new(e)} }
    rule global_def(ann: &TextInfo) -> GlobalDef
        = "static" b:binding(ann) ";" { GlobalDef{ name: b.0, ty: b.1} }
    rule function_def(ann: &TextInfo) -> FuncDef
        = inl:("inline" _)? "fn" _ name:identifier(ann) args:args_parens(ann) ret:("->" _ ty:typename(ann) {ty})? _ stt:code_block(ann)
        { FuncDef{name, inline: inl.is_some(), ret, args, stt} }
    rule import_def(ann: &TextInfo) -> Vec<Identifier>
        = "import" path:((_ i:identifier(ann) _ {i}) ** ".") _ ";" { path }

    // ************* Statements and blocks
    rule code_block(ann: &TextInfo) -> Vec<Statement>
        = n1:$("{") list:(statement(ann)*) n2:$("}") { list }
    rule statement(ann: &TextInfo) -> Statement
        = _ b:code_block(ann) _ { Statement::Block(b) }
        / _ "var" b:binding(ann) "=" e:expression(ann) ";" _ { Statement::Var {name: b.0, ty: b.1, expr: Box::new(e)} }
        / _ "while" c:expression(ann) block:code_block(ann) _ { Statement::While {cond: Box::new(c), block} }
        / _ "do" _ block:code_block(ann) c:expression(ann) ";" _ { Statement::DoWhile {block, cond: Box::new(c)} }
        / _ "loop" _ b:code_block(ann) _ { Statement::LoopForever(b) }
        / _ i:if_else(ann) _ { i }
        / _ n1:$("continue") _ n2:$(";") _ { Statement::Continue(ann.spanned(n1).merge(ann.spanned(n2))) }
        / _ n1:$("break") _ n2:$(";") _ { Statement::Break(ann.spanned(n1).merge(ann.spanned(n2))) }
        / _ n1:$("return") e:expression(ann)? _ n2:$(";") _ { Statement::Return(ann.spanned(n1).merge(ann.spanned(n2)), e.map(Box::new)) }
        / p:expression(ann) "=" v:expression(ann) { Statement::Assign(Box::new(p),Box::new(v)) }
        / e:expression(ann) _ ";" _ { Statement::Expr(Box::new(e)) }
    rule if_else(ann: &TextInfo) -> Statement
        = "if" c:expression(ann) block:code_block(ann) _ els:("else" _ b:code_block(ann) _ { b })?
        { Statement::If {cond: Box::new(c), block, els } }

    // ************* Expressions
    rule expression(ann: &TextInfo) -> Expression
        = _ exp:operation(ann) _ { exp }
    rule operation(ann: &TextInfo) -> Expression = precedence!{
        a:@ _ "==" _ b:(@) { Expression::Eq(Box::new(a),Box::new(b)) }
        a:@ _ "!=" _ b:(@) { Expression::Ne(Box::new(a),Box::new(b)) }
        --
        a:@ _ ">"  _ b:(@) { Expression::Gr(Box::new(a),Box::new(b)) }
        a:@ _ ">=" _ b:(@) { Expression::Ge(Box::new(a),Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Expression::Lo(Box::new(a),Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Expression::Le(Box::new(a),Box::new(b)) }
        --
        a:(@) _ "|" _ b:@ { Expression::Or(Box::new(a),Box::new(b)) }
        --
        a:(@) _ "^" _ b:@ { Expression::Xor(Box::new(a),Box::new(b)) }
        --
        a:(@) _ "&" _ b:@ { Expression::And(Box::new(a),Box::new(b)) }
        --
        a:(@) _ "<<" _ b:@ { Expression::Shl(Box::new(a),Box::new(b)) }
        a:(@) _ ">>" _ b:@ { Expression::Shr(Box::new(a),Box::new(b)) }
        a:(@) _ ">>>" _ b:@ { Expression::AriShr(Box::new(a),Box::new(b)) }
        --
        a:(@) _ "+" _ b:@ { Expression::Add(Box::new(a),Box::new(b)) }
        a:(@) _ "-" _ b:@ { Expression::Sub(Box::new(a),Box::new(b)) }
        --
        a:(@) _ "*" _ b:@ { Expression::Mul(Box::new(a),Box::new(b)) }
        --
        a:(@) _ "as" _ ty:typename(ann) { Expression::Cast(ty,Box::new(a)) }
        --
        "-" _ e:@ { Expression::Neg(Box::new(e)) }
        "!" _ e:@ { Expression::Not(Box::new(e)) }
        "*" _ e:@ { Expression::Pointer(Box::new(e)) }
        --
        e:(@) _ "[" _ idx:expression(ann) _ "]" { Expression::Index(Box::new(e), Box::new(idx)) }
        e:(@) _ "." _ name:identifier(ann) { Expression::AccessName(Box::new(e),name) }
        --
        n:identifier(ann) _ list:expr_parens(ann) { Expression::Call(n, list) }
        n:identifier(ann) { Expression::Name(n) }
        n:number_expr(ann) { n }
        s:string(ann) { Expression::String(s.0, s.1) }
        c:character(ann) { Expression::Character(c.0, c.1) }
        "(" _ e:expression(ann) _ ")" { Expression::Paren(Box::new(e)) }
        a:array(ann) { Expression::ArrayInit(a.0, a.1) }
    }

    rule number_expr(ann: &TextInfo) -> Expression
        = n:number(ann) {?
            let mut sign = false;
            let sub = if n.1.ends_with('u') {
                let mut c = n.1.chars();
                c.next_back().unwrap();
                c.as_str()
            } else if n.1.ends_with('i') {
                sign = true;
                let mut c = n.1.chars();
                c.next_back().unwrap();
                c.as_str()
            }else{ //default sign
                sign = false;
                n.1.as_str()
            };
            let number = sub.parse::<i64>().map_err(|_|"number literal")?;
            Ok(Expression::Number(number,sign,n.0))
        }
    rule args_parens(ann: &TextInfo) -> Vec<Argument>
        = _ "(" _ ")" _ { Vec::new() }
        / _ "(" _ list:(binding(ann) ** ",") _ ")" _ { list.into_iter().map(|(name,ty)| Argument{name, ty}).collect() }
    rule expr_parens(ann: &TextInfo) -> Vec<Expression>
        = _ "(" _ ")" _ { Vec::new() }
        / _ "(" _ list:(expression(ann) ** ",") _ ")" _ { list }
    rule array(ann: &TextInfo) -> (Span, Vec<Expression>)
        = n1:$("[") _ list:(expression(ann) ** ",") _ n2:$("]") { (ann.spanned(n1).merge(ann.spanned(n2)), list) }
    rule binding(ann: &TextInfo) -> (Identifier, Type)
        = _ i:identifier(ann) _ ":" _ t:typename(ann) _ { (i,t) }
    rule character(ann: &TextInfo) -> (Span, String)
        = quiet!{ n:$("'" ("\\'" / (!"'" [_]))* "'") { (ann.spanned(n), n.to_string()) }} / expected!("char literal")
    rule number(ann: &TextInfo) -> (Span, String)
        = quiet!{ n:$(_num_part() ("." _num_part())?) { (ann.spanned(n), n.to_string()) }} / expected!("number literal")
    rule _num_part() = ['0'..='9'] (['0'..='9'|'_'] / (['e'|'E'] ['-'|'+']? ) / ident_part())*
    rule string(ann: &TextInfo) -> (Span, String)
        = quiet!{ n:$("\"" ("\\\"" /(!"\"" [_]))* "\"") { (ann.spanned(n), n.to_string()) }} / expected!("string literal")
    rule typename(ann: &TextInfo) -> Type
        = quiet!{ _ n:$("i16") _ { Type::I16(ann.spanned(n)) }}
        / quiet!{_ n:$("u16") _ { Type::U16(ann.spanned(n)) }}
        / quiet!{_ n:$("i8") _ { Type::I8(ann.spanned(n)) }}
        / quiet!{_ n:$("u8") _ { Type::U8(ann.spanned(n)) }}
        / quiet!{_ n:$("bool") _ { Type::Bool(ann.spanned(n)) }}
        / quiet!{_ s:$("*") _ ty:typename(ann) _ { Type::Ptr(ann.spanned(s), Box::new(ty)) }}
        / expected!("type")

    rule identifier(ann: &TextInfo) -> Identifier
        = quiet!{ n:$(ident_start() ident_part()*) { Identifier{span:ann.spanned(n), value: n.to_string()} }}
        / expected!("identifier")
    // // helpers *******************************************************
    rule ident_start() = ch:$([_]) {? if is_label_start(ch.chars().next().unwrap()) {Ok(())} else {Err("Identifier start")}}
    rule ident_part() = ch:$([_]) {? if is_label_part(ch.chars().next().unwrap()) {Ok(())} else {Err("Identifier part")}}
    // ignorable *****************************************************
    rule _() =  quiet!{skip()}
    rule skip() = (whitespace() / new_line() / oneline_comment() / multiline_comment())*
    rule com_nl() = new_line() / oneline_comment() / multiline_comment()
    rule new_line() = "\r\n" / "\r" / "\n" / "\u{2028}" / "\u{2029}" / "\u{0085}"
    rule oneline_comment() = "//" (!new_line() [_])* new_line()
    rule multiline_comment() = "/*" (!"*/" [_])* "*/"
    rule whitespace() = ch:$([_]) {? if is_whitespace(ch.chars().next().unwrap()) {Ok(())} else {Err("whitespace")}}
});

fn run_parser(text: &str) -> Result<Vec<ItemOrImport>, AstError> {
    let info = TextInfo::new(text);
    match parser::top(text, &info) {
        Ok(v) => Ok(v),
        Err(err) => Err(AstError {
            span: info.spanned(&text[err.location.offset..err.location.offset]),
            expected: err.expected.tokens().collect(),
        }),
    }
}

pub fn parse_ast(main_file: &str, loader: Box<dyn SourceLoader>) -> (Result<Vec<Item>, Vec<AstError>>, SourceMap) {
    todo!()
}

#[cfg(test)]
mod test {
    use crate::mylang::ast::run_parser;

    #[test]
    fn test_parse_basic() {
        let text = r#"


        fn test_func(data: u16) -> u8 {
            var a: u16 = 123;
            return a;
        }
        "#;

        let src = run_parser(text).unwrap();

        println!("{src:#?}");
    }
}
