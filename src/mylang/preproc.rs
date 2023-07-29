use crate::*;
use druid::piet::TextStorage;
use std::fmt::{Debug, Display, Formatter, Write};
use std::sync::{Arc, Mutex};
use TokenKind::*;

#[derive(Clone)]
struct State {
    src: Arc<str>,
    index: usize,
}

#[derive(Clone)]
pub struct SourceMap {
    src: Arc<str>,
}

static GLOBAL_SOURCE: Mutex<Option<SourceMap>> = Mutex::new(None);

impl SourceMap {
    pub fn get_global() -> Option<SourceMap> {
        Option::clone(&*GLOBAL_SOURCE.lock().unwrap())
    }
    pub fn global() -> SourceMap {
        Self::get_global().unwrap()
    }
    pub fn set_global(map: SourceMap) {
        *GLOBAL_SOURCE.lock().unwrap() = Some(map);
    }
    pub fn set_new(src: Arc<str>) {
        Self::set_global(SourceMap { src })
    }
    pub fn get_source_for(&self, span: Span) -> Option<&str> {
        self.src.get(span.start..span.end)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn lo(self) -> usize {
        self.start
    }
    pub fn hi(self) -> usize {
        self.end
    }
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn contains(self, span: Self) -> bool {
        span.lo() >= self.lo() && span.hi() <= self.hi()
    }
    pub fn contains_point(self, pt: usize) -> bool {
        pt >= self.lo() && pt <= self.hi()
    }
    pub fn intersects(self, span: Self) -> bool {
        self.lo() <= span.hi() && self.hi() >= span.lo()
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
    pub fn is_kind(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
    pub fn is_value(&self, src: &SourceMap, value: &str) -> bool {
        src.get_source_for(self.span) == Some(value)
    }
    pub fn is(&self, src: &SourceMap, kind: TokenKind, value: &str) -> bool {
        self.is_kind(kind) && self.is_value(src, value)
    }
    pub fn is_special(&self, sp: KindSpec) -> bool {
        self.is_kind(TokenKind::Special(sp))
    }
    pub fn is_value_char(&self, src: &SourceMap, value: char) -> bool {
        let mut buff = [0u8; 4];
        self.is_value(src, value.encode_utf8(&mut buff))
    }
}
impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)?;
        if let Some(map) = SourceMap::get_global() {
            let src = map.get_source_for(self.span).unwrap();
            write!(f, "[{:?}", src)?;
            if self.kind == TokenKind::Whitespace(KindWhitespace::NewLine) {
                for c in src.chars() {
                    match c {
                        '\n' => write!(f, "\\n")?,
                        '\r' => write!(f, "\\r")?,
                        c => write!(f, "{}", c)?,
                    }
                }
                write!(f, "]")?;
            } else {
                write!(f, "{:?}]", src)?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum TokenKind {
    Whitespace(KindWhitespace), // (<whitespace>)
    Label(Option<KindKeyword>), // <letter> | "_" , (<label-char>)
    NumberLiteral(KindNumber),
    StringLiteral(bool),
    CharLiteral(bool),
    Comment,
    MultilineComment,
    Special(KindSpec), //alone special character
    //SpecialMult, //special character followed immediately by other special character
    EndOfStream,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum KindWhitespace {
    Regular,
    NewLine,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(u8)]
pub enum KindNumber {
    Clean,    //only digits
    NumWithE, //only digits but is finished with 'e' or 'E'
    Other,    //any other e.g '10u32', '123usize'
}

macro_rules! special_from_to{
    ($($lit:literal => $name:ident),*) => {
        #[derive(Copy,Clone,Eq,PartialEq,Debug,Hash)]
        #[repr(u8)]
        pub enum KindSpec{
            $(
            $name
            ),*
            ,Unknown // any other
        }
        impl KindSpec {
            pub const fn from_char(c: char)->Self{
                use KindSpec::*;
                match c {
                    $($lit => $name),*
                    ,_ => Unknown, // any other
                }
            }
            pub const fn to_char(self)->Option<char>{
                use KindSpec::*;
                match self {
                    Unknown => None,
                    $($name => Some($lit)),*
                }
            }
            pub const fn all()->&'static [Self]{
                &[ $(Self::$name),* ]
            }
        }
    }
}
macro_rules! keyword_from_to{
    ($($lit:literal => $name:ident),*) => {
        #[derive(Copy,Clone,Eq,PartialEq,Debug,Hash)]
        #[repr(u8)]
        pub enum KindKeyword{
            $($name),*
        }
        impl KindKeyword {
            pub fn from_str(s: &str)->Option<Self>{
                use KindKeyword::*;
                match s {
                    $($lit => Some($name)),*
                    ,_ => None, // any other
                }
            }
            pub const fn to_str(self)->&'static str{
                use KindKeyword::*;
                match self {
                    $($name => $lit),*
                }
            }
            pub const fn all()->&'static [Self]{
                &[ $(Self::$name),* ]
            }
        }
    }
}
special_from_to! {
    '(' => LParen, ')' => RParen,// ( )
    '{' => LBrace, '}' => RBrace,// { }
    '[' => LBracket,']' => RBracket,// [ ]
    '+' => Plus,'-' => Minus, // + -
    '#' => Hash,':' => Colon,';' => Semicolon, // # : ;
    '=' => EqSign,'<' => LoSign,'>' => GtSign, // = < >
    '|' => Or,'&' => And,'^' => Xor,'!' => Excl, // | & ^ !
    '*' => Asterisk,'/' => Div,'%' => Mod, // * / %
    '.' => Dot,',' => Comma,'`' =>Grave, // . , `
    '@' => At,'$' => Dollar, // @ $
    '~' => Tilde,'?' => Question, // ~ ?
    '\'' => SQuote,'"' => DQuote, // ' "
    '\\' => Backslash // / \
}
keyword_from_to! {
    "break" => Break,
    "continue" => Continue
}
impl KindSpec {
    pub const fn is_bracket(self) -> bool {
        self.is_left_bracket() || self.is_right_bracket()
    }
    pub const fn is_left_bracket(self) -> bool {
        use KindSpec::*;
        match self {
            LParen | LBrace | LBracket => true,
            _ => false,
        }
    }
    pub const fn is_right_bracket(self) -> bool {
        use KindSpec::*;
        match self {
            RParen | RBrace | RBracket => true,
            _ => false,
        }
    }
}

impl Display for KindKeyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}
impl Display for KindSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.to_char().unwrap_or(char::REPLACEMENT_CHARACTER))
    }
}

impl State {
    fn next(&mut self) -> Option<char> {
        self.src[self.index..].chars().next().map(|ch| {
            self.index += ch.len_utf8();
            ch
        })
    }
    fn back(&mut self) {
        let mut backing = self.src[..self.index].chars();
        backing.next_back();
        self.index = backing.as_str().len();
    }
    #[inline]
    fn span_from(&self, start: usize) -> Span {
        Span { start, end: self.index }
    }

    pub fn parse_next(&mut self) -> Token {
        let start = self.index;
        match self.next() {
            None => self.read_end_of_stream(),
            Some(c) => match c {
                c if is_whitespace(c) => self.read_whitespace(start),
                c if is_new_line(c) => self.read_new_line(start, c),
                c if is_label_start(c) => self.read_label(start),
                c if is_number_start(c) => self.read_number(start),
                DOUBLE_QUOTE => self.read_quoted(start, DOUBLE_QUOTE, StringLiteral),
                SINGLE_QUOTE => self.read_quoted(start, SINGLE_QUOTE, CharLiteral),
                COMMENT => self.read_any_comment_or_slash(start),
                c => self.read_special(start, c),
            },
        }
    }

    fn read_end_of_stream(&mut self) -> Token {
        Token { kind: EndOfStream, span: self.span_from(self.index) }
    }
    fn read_whitespace(&mut self, start: usize) -> Token {
        self.read_while(is_whitespace);
        Token { kind: Whitespace(KindWhitespace::Regular), span: self.span_from(start) }
    }
    fn read_label(&mut self, start: usize) -> Token {
        self.read_while(is_label_part);
        Token { kind: Label(KindKeyword::from_str(&self.src[start..self.index])), span: self.span_from(start) }
    }
    fn read_special(&mut self, start: usize, c: char) -> Token {
        //exactly one char
        Token { kind: Special(KindSpec::from_char(c)), span: self.span_from(start) }
    }
    fn read_new_line(&mut self, start: usize, curr: char) -> Token {
        if curr == CR {
            match self.next() {
                Some(LF) => {
                    return Token {
                        //windows line ending
                        kind: Whitespace(KindWhitespace::NewLine),
                        span: self.span_from(start),
                    };
                }
                None => {} //dont back on eos
                Some(_) => self.back(),
            }
        }
        Token { kind: Whitespace(KindWhitespace::NewLine), span: self.span_from(start) }
    }

    fn read_number(&mut self, start: usize) -> Token {
        self.read_while(is_number_part);
        //todo resolve KindNumber
        let value = &self.src[start..self.index];
        let mut chars = value.chars();
        let kind = match chars.next_back() {
            Some('e' | 'E') => {
                if chars.all(is_number_start) {
                    KindNumber::NumWithE
                } else {
                    KindNumber::Other
                }
            }
            Some(c) if is_number_start(c) => {
                if chars.all(is_number_start) {
                    KindNumber::Clean
                } else {
                    KindNumber::Other
                }
            }
            _ => KindNumber::Other,
        };
        Token { kind: NumberLiteral(kind), span: self.span_from(start) }
    }
    fn read_quoted(&mut self, start: usize, quote: char, to_kind: impl FnOnce(bool) -> TokenKind) -> Token {
        let mut escape = false;
        let term = loop {
            match self.next() {
                None => break false,
                Some(c) => {
                    if !escape {
                        if c == ESCAPE_CHAR {
                            escape = true;
                        } else if c == quote {
                            break true;
                        }
                    } else {
                        escape = false;
                    }
                }
            }
        };
        return Token { kind: to_kind(term), span: self.span_from(start) };
    }
    fn read_any_comment_or_slash(&mut self, start: usize) -> Token {
        match self.next() {
            None => Token { kind: Special(KindSpec::Div), span: self.span_from(start) },
            Some(COMMENT) => {
                loop {
                    match self.next() {
                        None => break,
                        Some(c) if is_new_line(c) => break,
                        _ => {}
                    }
                }
                self.back();
                Token { kind: Comment, span: self.span_from(start) }
            }
            Some(MULTILINE_COMMENT_STAR) => {
                let mut prev_star = false;
                let mut prev_cr = false;
                let mut curr;
                loop {
                    curr = match self.next() {
                        None => {
                            //self.back(); //todo check
                            break;
                        }
                        Some(c) => c,
                    };
                    if curr == LF {
                        if prev_cr {
                            prev_cr = false; //no increment line cause cr already did it
                        }
                    } else if curr == CR {
                        prev_cr = true;
                    } else if is_new_line(curr) {
                        prev_cr = false;
                    } else {
                        prev_cr = false;
                    }
                    if prev_star {
                        if curr == COMMENT {
                            break;
                        }
                        prev_star = false;
                    } else {
                        if curr == MULTILINE_COMMENT_STAR {
                            prev_star = true;
                        }
                    }
                }
                Token { kind: MultilineComment, span: self.span_from(start) }
            }
            Some(_) => {
                self.back();
                Token { kind: Special(KindSpec::Div), span: self.span_from(start) }
            }
        }
    }
    fn read_while<F: Fn(char) -> bool>(&mut self, func: F) {
        loop {
            match self.next() {
                None => break,
                Some(c) if !func(c) => {
                    self.back();
                    break;
                }
                Some(_) => {}
            }
        }
    }
}

//helper functions and consts
const CR: char = '\r';
const LF: char = '\n';
const PLUS: char = '+';
const MINUS: char = '-';
const DOUBLE_QUOTE: char = '"';
const SINGLE_QUOTE: char = '\'';
const ESCAPE_CHAR: char = '\\';
const COMMENT: char = '/';
const MULTILINE_COMMENT_STAR: char = '*';

fn is_new_line(c: char) -> bool {
    c == LF || c == CR || c == '\u{2028}' || c == '\u{2029}' || c == '\u{0085}'
}
pub fn is_whitespace(c: char) -> bool {
    if is_new_line(c) {
        return false;
    } //exclude new line chars
    let c = c as u32;
    // Any ASCII whitespace character?
    if (c >= 0x1c && c <= 0x20) || (c >= 0x09 && c <= 0x0d) {
        return true;
    }
    if c < 0x1000 {
        return false;
    }
    // OGHAM SPACE MARK or MONGOLIAN VOWEL SEPARATOR?
    if c == 0x1680 || c == 0x180e {
        return true;
    }
    if c < 0x2000 {
        return false;
    }
    // Exclude General Punctuation's non-breaking spaces (which includes FIGURE SPACE).
    if c == 0x2007 || c == 0x202f {
        return false;
    }
    // Other whitespace from General Punctuation...
    c <= 0x200a || c == 0x2028 || c == 0x2029 || c == 0x205f || c == 0x3000 // ...or CJK Symbols and Punctuation?
}
fn is_letter(c: char) -> bool {
    c.is_alphabetic()
}
fn is_label_start(c: char) -> bool {
    is_letter(c) || c == '_'
}
fn is_label_part(c: char) -> bool {
    is_label_start(c) || c.is_numeric()
}
fn is_number_start(c: char) -> bool {
    ('0'..='9').contains(&c)
}
fn is_number_part(c: char) -> bool {
    is_number_start(c) || is_letter(c) || c == '_'
}
fn is_exponent_spec(c: char) -> bool {
    c == 'e' || c == 'E'
}
fn is_point_sep(c: char) -> bool {
    c == '.'
}

#[derive(Clone)]
pub struct TokenIter {
    state: State,
    skip_eos: bool,
}

impl Iterator for TokenIter {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.state.parse_next(); //always returns token, when eos - will repeat eos tokens.
        if tok.kind == EndOfStream {
            if self.skip_eos {
                return None;
            }
            self.skip_eos = true;
        }
        Some(tok)
    }
}

pub fn token_iter(src: Arc<str>, skip_eos: bool) -> TokenIter {
    assert!(src.as_str().len() <= i32::MAX as _); //max 2G of text
    TokenIter { state: State { index: 0, src }, skip_eos }
}
