use std::fmt::{Debug, Write};

#[macro_export]
macro_rules! match_for {
    ($val:expr => $($vals:expr),+) => {{
        let expr = $val;
        $(*expr == $vals)||+
    }};
}

#[macro_export]
macro_rules! sym {
    ($str:expr) => {
        Token::Symbol($str.to_string(), 0, 0)
    };
}

#[macro_export]
macro_rules! ident {
    ($str:expr) => {
        Token::Ident($str.to_string(), 0, 0)
    };
}

#[derive(Clone)]
pub enum Token {
    Ident(String, usize, usize),
    Int(i64, usize, usize),
    Float(f64, usize, usize),
    String(String, usize, usize),
    Symbol(String, usize, usize),

    Eof,
}

impl Token {
    pub fn is(&self, ty: &str) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match (self, ty) {
            (Self::Ident(..), "ident") => true,
            (Self::Int(..), "int") => true,
            (Self::Float(..), "float") => true,
            (Self::String(..), "string") => true,
            (Self::Symbol(..), "symbol") => true,
            _ => false,
        }
    }

    pub fn row(&self) -> usize {
        match *self {
            Self::Ident(_, r, _) => r,
            Self::Int(_, r, _) => r,
            Self::Float(_, r, _) => r,
            Self::String(_, r, _) => r,
            Self::Symbol(_, r, _) => r,
            Self::Eof => 0,
        }
    }

    pub fn col(&self) -> usize {
        match *self {
            Self::Ident(_, _, c) => c,
            Self::Int(_, _, c) => c,
            Self::Float(_, _, c) => c,
            Self::String(_, _, c) => c,
            Self::Symbol(_, _, c) => c,
            Self::Eof => 0,
        }
    }

    pub fn prec(&self) -> i8 {
        if match_for!(self => sym!("&&"), sym!("||")) {
            1
        } else if match_for!( self => sym!("=="), sym!("!="), sym!(">="), sym!(">"), sym!("<="), sym!("<") ) {
            2
        } else if *self == sym!("%") {
            3
        } else if match_for!( self => sym!("+"), sym!("-")) {
            4
        } else if match_for!( self => sym!("/"), sym!("*"), sym!("//")) {
            5
        } else if *self == sym!("??") {
            6
        } else {
            -1
        }
    }

    pub fn value(&self) -> String {
        match self {
            Self::Ident(i, row, col) => i.clone(),
            Self::Int(n, row, col) => n.to_string(),
            Self::Float(v, row, col) => v.to_string(),
            Self::String(s, row, col) => s.clone(),
            Self::Symbol(s, row, col) => s.clone(),
            Self::Eof => "\0".to_string(),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ident(s, ..), Self::Ident(o, ..)) => s == o,
            (Self::Int(s, ..), Self::Int(o, ..)) => s == o,
            (Self::Float(s, ..), Self::Float(o, ..)) => s == o,
            (Self::String(s, ..), Self::String(o, ..)) => s == o,
            (Self::Symbol(s, ..), Self::Symbol(o, ..)) => s == o,
            (Self::Eof, Self::Eof) => true,
            _ => false,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(i, row, col) => f.write_fmt(format_args!("IDENT  : {i} => {row}:{col}")),
            Self::Int(n, row, col) => f.write_fmt(format_args!("NUMBER : {n} => {row}:{col}")),
            Self::Float(v, row, col) => f.write_fmt(format_args!("FLOAT  : {v}F => {row}:{col}")),
            Self::String(s, row, col) => f.write_fmt(format_args!("STRING : '{s}' => {row}:{col}")),
            Self::Symbol(s, row, col) => f.write_fmt(format_args!("SYMBOL : {s} => {row}:{col}")),
            Self::Eof => f.write_str("<EOF>"),
        }
    }
}

pub fn lex(code: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut i = 0;
    let mut row = 1;
    let mut col = 1;
    let src: Vec<char> = code.chars().collect();

    macro_rules! push_tok {
        ($val:expr) => {{
            tokens.push(Token::Symbol($val.to_string(), row, col));
            i += 1;
            col += 1;
            continue;
        }};
    }

    while src.len() > i {
        if src[i] == '\n' {
            col = 1;
            row += 1;
            i += 1;
        } else if src[i] == '#' {
            let mut hashtag_count = 1;
            i += 1;
            while src.len() > i && src[i] == '#' {
                hashtag_count += 1;
                i += 1;
            }

            if hashtag_count > 2 {
                let mut end_count = 0;
                let mut last_match = false;
                while src.len() > i && end_count != hashtag_count {
                    if src[i] == '#' && last_match {
                        end_count += 1;
                    } else if src[i] == '#' {
                        end_count = 1;
                        last_match = true;
                    } else {
                        last_match = false;
                    }
                    if src[i] == '\n' {
                        col = 0;
                        row += 1;
                    }
                    col += 1;
                    i += 1;
                }
                while src.len() > i && src[i] == '#' {
                    i += 1;
                }
            } else {
                while src.len() > i && src[i] != '\n' {
                    i += 1;
                }
                i += 1;
                col = 1;
                row += 1;
            }
        } else if src[i].is_whitespace() {
            col += 1;
            i += 1
        } else if src[i] == '"' {
            let mut buf = String::new();
            col += 1;
            i += 1;
            while src.len() > i && src[i] != '"' {
                let ch = if src[i] == '\\' {
                    i += 1;
                    match src[i] {
                        't' => '\t',
                        'n' => '\n',
                        '\\' => '\\',
                        'e' => '\x1b',
                        c => c,
                    }
                } else {
                    src[i]
                };
                buf.push(ch);
                i += 1;
                col += 1;
            }
            i += 1;
            col += 1;
            tokens.push(Token::String(buf, row, col));
        } else if src[i].is_numeric() {
            let mut buf = String::new();
            let mut float = false;

            while src.len() > i && src[i].is_numeric() {
                buf.push(src[i]);
                i += 1;
                col += 1;
            }

            if src.len() > i && src[i] == '.' {
                buf.push('.');
                i += 1;
                col += 1;
                float = true;
            }

            while src.len() > i && src[i].is_numeric() && float {
                buf.push(src[i]);
                i += 1;
                col += 1;
            }

            if float {
                tokens.push(Token::Float(buf.parse::<f64>().unwrap(), row, col));
            } else {
                tokens.push(Token::Int(buf.parse::<i64>().unwrap(), row, col));
            }
        } else if src[i].is_alphabetic() {
            let mut buf = String::new();
            while src.len() > i && (src[i].is_alphanumeric() || src[i] == '_') {
                buf.push(src[i]);
                i += 1;
                col += 1;
            }
            tokens.push(Token::Ident(buf, row, col));
        } else {
            match src[i] {
                '(' => push_tok!("("),
                ')' => push_tok!(")"),
                '{' => push_tok!("{"),
                '}' => push_tok!("}"),
                '[' => push_tok!("["),
                ']' => push_tok!("]"),
                ';' => push_tok!(";"),
                _ => {}
            }

            let mut buf = String::new();
            while src.len() > i && !src[i].is_whitespace() && !src[i].is_alphanumeric() {
                if match_for!( &src[i] => '(', ')', '{', '}', '[', ']', ';', '"' ) {break}
                buf.push(src[i]);
                i += 1;
                col += 1;
            }
            tokens.push(Token::Symbol(buf, row, col));
        }
    }

    tokens.push(Token::Eof);
    tokens
}
