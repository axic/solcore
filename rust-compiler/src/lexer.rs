// SAIL lexer.
//
// Produces a stream of tokens. Skips whitespace and `//` line / `/* */`
// block comments. The grammar follows doc/src/sail/syntax.md.

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    // Keywords
    Pragma,
    Import,
    Export,
    As,
    Hiding,
    From,
    Contract,
    Function,
    Forall,
    Class,
    Instance,
    Default,
    Data,
    Type,
    Let,
    Match,
    If,
    Else,
    Return,
    Assembly,
    True,
    False,
    For,
    While,
    Case,
    Switch,
    Break,
    Continue,
    Leave,
    Constructor,

    // Punctuation / operators
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semi,
    Colon,
    Dot,
    Arrow,        // ->
    FatArrow,     // =>
    Walrus,       // :=
    Eq,           // =
    EqEq,         // ==
    NotEq,        // !=
    Lt,           // <
    Gt,           // >
    LtEq,         // <=
    GtEq,         // >=
    AndAnd,       // &&
    OrOr,         // ||
    Bang,         // !
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusEq,
    MinusEq,
    Pipe,
    At,
    Underscore,

    // Literals / identifiers
    Ident(String),
    Int(String),      // raw textual form (decimal or 0x...)
    Str(String),

    Eof,
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tok: Tok,
    pub line: usize,
    pub col: usize,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    line: usize,
    col: usize,
}

#[derive(Debug)]
pub struct LexError {
    pub msg: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lex error at {}:{}: {}", self.line, self.col, self.msg)
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0, line: 1, col: 1 }
    }

    fn peek(&self, n: usize) -> Option<u8> {
        self.src.get(self.pos + n).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let b = self.peek(0)?;
        self.pos += 1;
        if b == b'\n' { self.line += 1; self.col = 1; } else { self.col += 1; }
        Some(b)
    }

    fn skip_ws_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            match self.peek(0) {
                Some(b) if b.is_ascii_whitespace() => { self.bump(); }
                Some(b'/') if self.peek(1) == Some(b'/') => {
                    while let Some(b) = self.peek(0) {
                        if b == b'\n' { break; }
                        self.bump();
                    }
                }
                Some(b'/') if self.peek(1) == Some(b'*') => {
                    let (sl, sc) = (self.line, self.col);
                    self.bump(); self.bump();
                    loop {
                        match self.peek(0) {
                            None => return Err(LexError {
                                msg: "unterminated /* block comment".into(),
                                line: sl, col: sc,
                            }),
                            Some(b'*') if self.peek(1) == Some(b'/') => {
                                self.bump(); self.bump();
                                break;
                            }
                            Some(_) => { self.bump(); }
                        }
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_ws_and_comments()?;
        let line = self.line;
        let col = self.col;
        let b = match self.peek(0) {
            None => return Ok(Token { tok: Tok::Eof, line, col }),
            Some(b) => b,
        };

        // Multi-character operators
        macro_rules! two {
            ($a:expr, $b:expr, $tok:expr) => {
                if b == $a && self.peek(1) == Some($b) {
                    self.bump(); self.bump();
                    return Ok(Token { tok: $tok, line, col });
                }
            };
        }
        two!(b'-', b'>', Tok::Arrow);
        two!(b'=', b'>', Tok::FatArrow);
        two!(b':', b'=', Tok::Walrus);
        two!(b'=', b'=', Tok::EqEq);
        two!(b'!', b'=', Tok::NotEq);
        two!(b'<', b'=', Tok::LtEq);
        two!(b'>', b'=', Tok::GtEq);
        two!(b'&', b'&', Tok::AndAnd);
        two!(b'|', b'|', Tok::OrOr);
        two!(b'+', b'=', Tok::PlusEq);
        two!(b'-', b'=', Tok::MinusEq);

        // Single-character punctuation
        let single = |t: Tok, this: &mut Lexer| { this.bump(); Token { tok: t, line, col } };
        match b {
            b'(' => return Ok(single(Tok::LParen, self)),
            b')' => return Ok(single(Tok::RParen, self)),
            b'{' => return Ok(single(Tok::LBrace, self)),
            b'}' => return Ok(single(Tok::RBrace, self)),
            b'[' => return Ok(single(Tok::LBracket, self)),
            b']' => return Ok(single(Tok::RBracket, self)),
            b',' => return Ok(single(Tok::Comma, self)),
            b';' => return Ok(single(Tok::Semi, self)),
            b':' => return Ok(single(Tok::Colon, self)),
            b'.' => return Ok(single(Tok::Dot, self)),
            b'=' => return Ok(single(Tok::Eq, self)),
            b'<' => return Ok(single(Tok::Lt, self)),
            b'>' => return Ok(single(Tok::Gt, self)),
            b'!' => return Ok(single(Tok::Bang, self)),
            b'+' => return Ok(single(Tok::Plus, self)),
            b'-' => return Ok(single(Tok::Minus, self)),
            b'*' => return Ok(single(Tok::Star, self)),
            b'/' => return Ok(single(Tok::Slash, self)),
            b'%' => return Ok(single(Tok::Percent, self)),
            b'|' => return Ok(single(Tok::Pipe, self)),
            b'@' => return Ok(single(Tok::At, self)),
            _ => {}
        }

        // String literal
        if b == b'"' {
            self.bump();
            let mut s = String::new();
            loop {
                match self.peek(0) {
                    None => return Err(LexError { msg: "unterminated string".into(), line, col }),
                    Some(b'"') => { self.bump(); break; }
                    Some(b'\\') => {
                        self.bump();
                        match self.bump() {
                            Some(b'n') => s.push('\n'),
                            Some(b't') => s.push('\t'),
                            Some(b'"') => s.push('"'),
                            Some(b'\\') => s.push('\\'),
                            Some(c) => s.push(c as char),
                            None => return Err(LexError { msg: "unterminated string".into(), line, col }),
                        }
                    }
                    Some(c) => { s.push(c as char); self.bump(); }
                }
            }
            return Ok(Token { tok: Tok::Str(s), line, col });
        }

        // Integer literal
        if b.is_ascii_digit() {
            let start = self.pos;
            if b == b'0' && self.peek(1) == Some(b'x') {
                self.bump(); self.bump();
                while let Some(c) = self.peek(0) {
                    if c.is_ascii_hexdigit() || c == b'_' { self.bump(); } else { break; }
                }
            } else {
                while let Some(c) = self.peek(0) {
                    if c.is_ascii_digit() || c == b'_' { self.bump(); } else { break; }
                }
            }
            let s = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
            return Ok(Token { tok: Tok::Int(s), line, col });
        }

        // Identifier / keyword
        if b.is_ascii_alphabetic() || b == b'_' {
            let start = self.pos;
            while let Some(c) = self.peek(0) {
                if c.is_ascii_alphanumeric() || c == b'_' { self.bump(); } else { break; }
            }
            let s = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
            let tok = match s {
                "pragma" => Tok::Pragma,
                "import" => Tok::Import,
                "export" => Tok::Export,
                "as" => Tok::As,
                "hiding" => Tok::Hiding,
                "from" => Tok::From,
                "contract" => Tok::Contract,
                "function" => Tok::Function,
                "forall" => Tok::Forall,
                "class" => Tok::Class,
                "instance" => Tok::Instance,
                "default" => Tok::Default,
                "data" => Tok::Data,
                "type" => Tok::Type,
                "let" => Tok::Let,
                "match" => Tok::Match,
                "if" => Tok::If,
                "else" => Tok::Else,
                "return" => Tok::Return,
                "assembly" => Tok::Assembly,
                "true" => Tok::True,
                "false" => Tok::False,
                "for" => Tok::For,
                "while" => Tok::While,
                "case" => Tok::Case,
                "switch" => Tok::Switch,
                "break" => Tok::Break,
                "continue" => Tok::Continue,
                "leave" => Tok::Leave,
                "constructor" => Tok::Constructor,
                "_" => Tok::Underscore,
                _ => Tok::Ident(s.to_string()),
            };
            return Ok(Token { tok, line, col });
        }

        Err(LexError { msg: format!("unexpected byte 0x{:02x} ({:?})", b, b as char), line, col })
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            let t = self.next_token()?;
            let done = matches!(t.tok, Tok::Eof);
            out.push(t);
            if done { return Ok(out); }
        }
    }
}
