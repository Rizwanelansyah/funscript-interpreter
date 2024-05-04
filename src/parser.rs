use crate::{error, format_err, ident, lexer::Token, match_for, sym};

macro_rules! cur {
    ($self:expr) => {
        if $self.tokens.len() <= $self.i {
            Token::Eof
        } else {
            $self.tokens[$self.i].clone()
        }
    };
}

macro_rules! off {
    ($self:expr; $off:expr) => {
        $self.tokens[($self.i as isize + $off) as usize]
    };
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, Vec<String>> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    error: bool,
    i: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            i: 0,
            error: false,
        }
    }

    pub fn error(&mut self, mode: bool) {
        self.error = mode;
    }

    pub fn parse(&mut self) -> Result<Program, Vec<String>> {
        let mut stmts = vec![];
        let mut errors = vec![];
        let mut returning = false;

        while cur!(self) != Token::Eof {
            match self.parse_stmt() {
                Ok(v) => stmts.push(v),
                Err(e) => {
                    if self.error {
                        error(1, &e);
                    } else {
                        errors.push(e)
                    }
                }
            }
            if cur!(self) == Token::Eof {
                returning = true;
                break;
            }
            if let Err(e) = self.try_is(sym!(";"), "expected semicolon") {
                errors.push(e);
            };
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        if !returning {
            stmts.push(Stmt::Empty);
        }

        Ok(Program {
            block: Block { stmts },
        })
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, String> {
        if cur!(self) == ident!("print") {
            self.i += 1;
            let expr = self.parse_expr(1)?;
            return Ok(Stmt::Print(expr));
        } else if cur!(self) == ident!("let") && off!(self; 1).is("ident") {
            self.i += 1;
            let name = cur!(self);
            self.i += 1;
            let mut expr = Expr::Term(Term::Nil);
            if cur!(self) == sym!("=") {
                self.i += 1;
                expr = self.parse_expr(1)?;
            }
            return Ok(Stmt::VariableDefinition { name, expr });
        } else if cur!(self) == ident!("return") {
            self.i += 1;
            let expr = self.parse_expr(1)?;
            return Ok(Stmt::Return(expr));
        } else if cur!(self) == ident!("break") {
            self.i += 1;
            if cur!(self) == sym!(":") && off!(self; 1).is("ident") {
                self.i += 1;
                let lable = cur!(self);
                self.i += 1;
                let expr = self.parse_expr(1)?;
                return Ok(Stmt::Break(lable, expr));
            }
            let expr = self.parse_expr(1)?;
            return Ok(Stmt::Break(Token::Eof, expr));
        }
        let expr = self.parse_expr(1)?;
        Ok(Stmt::Expr(expr))
    }

    pub fn parse_expr(&mut self, min_prec: i8) -> Result<Expr, String> {
        if cur!(self) == sym!("{") {
            return Ok(Expr::Block(self.parse_block()?));
        }
        if cur!(self).is("ident") && off!(self; 1) == sym!("=") {
            let name = cur!(self);
            self.i += 2;
            let expr = self.parse_expr(1)?;
            return Ok(Expr::VariableAssignment {
                name,
                expr: Box::new(expr),
            });
        } else if cur!(self) == ident!("if") {
            return self.parse_conditional();
        } else if cur!(self) == ident!("while") {
            return self.parse_loop();
        } else if cur!(self) == sym!(":") && off!(self; 1).is("ident") && off!(self; 2) == sym!(":")
        {
            return self.parse_labled();
        }

        let mut val: Option<Expr> = None;
        if match_for!(&cur!(self) => sym!("-"), sym!("!")) {
            let op = cur!(self);
            self.i += 1;
            let expr = self.parse_expr(1)?;
            val = Some(Expr::Unary {
                expr: Box::new(expr),
                op,
            });
        }
        if cur!(self) == sym!("(") {
            self.i += 1;
            let expr = self.parse_expr(1)?;
            self.try_is(sym!(")"), "expected close parent")?;
            val = Some(expr);
        }

        if val.is_none() {
            val = Some(Expr::Term(self.parse_term()?));
        }

        loop {
            let op = cur!(self);
            if op.prec() >= min_prec {
                self.i += 1;
                let rhs = self.parse_expr(op.prec())?;
                val = Some(Expr::Bin {
                    lhs: Box::new(val.unwrap()),
                    op,
                    rhs: Box::new(rhs),
                });
            } else {
                break;
            }
        }

        if cur!(self) == sym!("?") {
            self.i += 1;
            val = Some(Expr::Unary {
                op: sym!("?"),
                expr: Box::new(val.unwrap()),
            });
        }

        Ok(val.unwrap())
    }

    pub fn parse_conditional(&mut self) -> Result<Expr, String> {
        let pos = cur!(self);
        self.i += 1;
        let cond = self.parse_expr(1)?;
        self.try_is(sym!(":"), "expected colon")?;
        let body = self.parse_stmt()?;
        if cur!(self) == ident!("else") {
            self.i += 1;
            let else_block = self.parse_stmt()?;
            Ok(Expr::ConditionalExpression {
                cond: Box::new(cond),
                body: Box::new(body),
                else_block: Box::new(else_block),
                loc: (pos.row(), pos.col()),
            })
        } else {
            Ok(Expr::ConditionalExpression {
                cond: Box::new(cond),
                body: Box::new(body),
                else_block: Box::new(Stmt::Expr(Expr::Term(Term::Nil))),
                loc: (pos.row(), pos.col()),
            })
        }
    }

    pub fn parse_labled(&mut self) -> Result<Expr, String> {
        self.i += 1;
        let id = cur!(self);
        self.i += 2;
        if cur!(self) == ident!("if") {
            return Ok(Expr::Labled(id, Box::new(self.parse_conditional()?)));
        } else if cur!(self) == ident!("while") {
            return Ok(Expr::Labled(id, Box::new(self.parse_loop()?)));
        }
        Ok(Expr::Labled(id, Box::new(Expr::Block(self.parse_block()?))))
    }

    pub fn parse_loop(&mut self) -> Result<Expr, String> {
        let pos = cur!(self);
        self.i += 1;
        let cond = self.parse_expr(1)?;
        self.try_is(sym!(":"), "expected colon")?;
        let body = self.parse_stmt()?;
        Ok(Expr::LoopExpression {
            cond: Box::new(cond),
            body: Box::new(body),
            loc: (pos.row(), pos.col()),
        })
    }

    pub fn parse_term(&mut self) -> Result<Term, String> {
        let cur = &cur!(self);
        self.i += 1;
        match cur {
            Token::String(s, ..) => Ok(Term::String(s.clone())),
            Token::Ident(ref id, ..) => match id.as_str() {
                "true" => Ok(Term::Bool(true)),
                "false" => Ok(Term::Bool(false)),
                "nil" => Ok(Term::Nil),
                _ => Ok(Term::Ident(cur.clone())),
            },
            Token::Int(i, ..) => Ok(Term::Int(*i)),
            Token::Float(f, ..) => Ok(Term::Float(*f)),
            _ => Err(format!(
                "at {}:{} => unknown termination",
                cur.row(),
                cur.col()
            )),
        }
    }

    pub fn try_is(&mut self, tok: Token, msg: &str) -> Result<(), String> {
        if cur!(self) != tok {
            return Err(format!(
                "at {}:{} => {msg}",
                off!(self; -1).row(),
                off!(self; -1).col()
            ));
        }
        self.i += 1;
        Ok(())
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        let mut stmts = vec![];
        let mut returning = false;
        self.try_is(sym!("{"), "expected open curly brace")?;
        while cur!(self) != Token::Eof && cur!(self) != sym!("}") {
            stmts.push(self.parse_stmt()?);
            if cur!(self) == sym!("}") {
                returning = true;
                break;
            }
            self.try_is(sym!(";"), "expected semicolon")?;
        }
        self.try_is(sym!("}"), "expected close curly brace")?;
        if !returning {
            stmts.push(Stmt::Empty);
        }
        Ok(Block { stmts })
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VariableDefinition { name: Token, expr: Expr },
    Print(Expr),
    Expr(Expr),
    Return(Expr),
    Break(Token, Expr),
    Empty,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Term(Term),
    Bin {
        lhs: Box<Expr>,
        op: Token,
        rhs: Box<Expr>,
    },
    Unary {
        expr: Box<Expr>,
        op: Token,
    },
    VariableAssignment {
        name: Token,
        expr: Box<Expr>,
    },
    Block(Block),
    ConditionalExpression {
        cond: Box<Expr>,
        body: Box<Stmt>,
        else_block: Box<Stmt>,
        loc: (usize, usize),
    },
    LoopExpression {
        cond: Box<Expr>,
        body: Box<Stmt>,
        loc: (usize, usize),
    },
    Labled(Token, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Term {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Ident(Token),
    Nil,
}
