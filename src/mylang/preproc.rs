use crate::mylang::SourceLoader;
use druid::piet::TextStorage;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write};
use std::panic::Location;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
struct State {
    src: Arc<str>,
    index: usize,
}

#[derive(Clone)]
pub struct SourceMap {
    inner: Arc<Mutex<SourceMapInner>>,
}
#[derive(Clone)]
pub struct Source {
    src: Arc<(Arc<str>, TextInfo)>,
}

struct SourceMapInner {
    loader: Box<dyn SourceLoader>,
    map: HashMap<String, Source>,
}

impl SourceMap {
    pub fn new(loader: Box<dyn SourceLoader>) -> Self {
        Self { inner: Arc::new(Mutex::new(SourceMapInner { loader, map: Default::default() })) }
    }

    pub fn get_source(&self, name: &str) -> Option<Source> {
        let mut inner = self.inner.lock().unwrap_or_else(|v| v.into_inner());
        if let Some(source) = inner.map.get(name).cloned() {
            return Some(source);
        }
        let source = inner.loader.load_source(name)?;
        let info = TextInfo::new(&source);
        let source = Source { src: Arc::new((source, info)) };
        inner.map.insert(name.to_string(), source.clone());
        Some(source)
    }

    pub fn get_source_for(&self, span: Span) -> Option<Source> {
        let inner = self.inner.lock().unwrap_or_else(|v| v.into_inner());
        inner
            .map
            .values()
            .find(|s| {
                let ptr = s.as_str().as_ptr() as usize;
                let end = ptr + s.as_str().len();
                (ptr..end).contains(&span.start)
            })
            .cloned()
    }
}

impl Source {
    pub fn new_mocked(text: &str) -> Self {
        let text: Arc<str> = text.to_string().into();
        let info = TextInfo::new(&text);
        Self { src: Arc::new((text, info)) }
    }

    pub fn as_str(&self) -> &str {
        self.src.0.as_str()
    }

    pub fn info(&self) -> &TextInfo {
        &self.src.1
    }

    pub fn str_for(&self, span: Span) -> Option<&str> {
        let ptr = self.src.0.as_ptr() as usize;
        let end = ptr + self.src.0.len();
        if !(ptr..end).contains(&span.start) {
            return None;
        }
        let offset = span.start - ptr;
        Some(&self.src.0[offset..(offset + span.len())])
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
    pub fn merge(&self, other: Self) -> Self {
        Self { start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}

pub struct TextInfo {
    start_ptr: usize,
    new_lines: Vec<usize>,
    len: usize,
}

impl TextInfo {
    pub fn new(text: &str) -> Self {
        let new_lines = text.char_indices().filter(|(_, c)| *c == '\n').map(|(i, _)| i).collect();
        Self { start_ptr: text.as_ptr() as usize, len: text.len(), new_lines }
    }

    pub fn try_spanned(&self, substring: &str) -> Option<Span> {
        let start = self.start_ptr;
        let sub = substring.as_ptr() as usize;
        if sub >= self.start_ptr && self.start_ptr + self.len >= sub + substring.len() {
            let start = sub - self.start_ptr;
            Some(Span { start, end: start + substring.len() })
        } else {
            None
        }
    }

    pub fn start_line_col(&self, span: Span) -> (u32, u32) {
        self.calculate_line_col(span.lo()).expect("Not part of source text")
    }
    pub fn end_line_col(&self, span: Span) -> (u32, u32) {
        self.calculate_line_col(span.hi()).expect("Not part of source text")
    }
    fn calculate_line_col(&self, ptr: usize) -> Option<(u32, u32)> {
        let ptr = ptr - self.start_ptr;
        if ptr >= self.len {
            return None;
        }
        let line_idx = self.new_lines.binary_search(&ptr).unwrap_or_else(|i| i);
        let line_start = match line_idx {
            0 => 0,
            v => self.new_lines[v - 1] + 1,
        };
        assert!(line_start <= ptr);
        Some(((line_idx + 1) as _, (ptr - line_start) as _))
    }

    pub fn spanned(&self, substring: &str) -> Span {
        self.try_spanned(substring).expect("Substring is not a part of original text")
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
pub fn is_letter(c: char) -> bool {
    c.is_alphabetic()
}
pub fn is_label_start(c: char) -> bool {
    is_letter(c) || c == '_'
}
pub fn is_label_part(c: char) -> bool {
    is_label_start(c) || c.is_numeric()
}
pub fn is_number_start(c: char) -> bool {
    ('0'..='9').contains(&c)
}
pub fn is_number_part(c: char) -> bool {
    is_number_start(c) || is_letter(c) || c == '_'
}
pub fn is_exponent_spec(c: char) -> bool {
    c == 'e' || c == 'E'
}
pub fn is_point_sep(c: char) -> bool {
    c == '.'
}
