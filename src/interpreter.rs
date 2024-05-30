use std::collections::HashMap;
use std::io::Write;

use crate::{
    error,
    lexer::Token,
    parser::{Expr, Program, Stmt, Term},
    sym,
};

macro_rules! env {
    () => {
        HashMap::new()
    };
    ($($key:ident: $value:expr),+ $(,)?) => {{
        let mut env: Env = HashMap::new();
        $(
            env.insert(stringify!($key).to_string(), $value);
         ),+
        env
    }};
}

pub type Env = HashMap<String, Value>;

pub struct Interpreter {
    builtin: Env,
    envs: Vec<Env>,
    error: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        let builtin = env! {};
        Self {
            builtin,
            envs: vec![env! {}],
            error: false,
        }
    }

    pub fn error(&mut self, mode: bool) {
        self.error = mode;
    }

    pub fn exec(&mut self, program: &Program) -> Result<Value, String> {
        let mut ret = Value::Nil;
        for stmt in program.block.stmts.iter() {
            ret = match self.exec_stmt(stmt) {
                Ok(it) => it,
                Err(err) => {
                    if self.error {
                        error(1, &err);
                    } else {
                        return Err(err);
                    }
                }
            };
            if self.envs.is_empty() {
                return Ok(ret);
            }
        }
        Ok(ret)
    }

    pub fn exec_stmt(&mut self, stmt: &Stmt) -> Result<Value, String> {
        match stmt {
            Stmt::Expr(expr) => self.exec_expr(expr),
            Stmt::VariableDefinition { name, expr } => {
                if self.envs.last().unwrap().get(&name.value()).is_some() {
                    return Err(format!(
                        "at {}:{} => variable with name '{}' is already defined in this scope",
                        name.row(),
                        name.col(),
                        name.value()
                    ));
                }
                let val = self.exec_expr(expr)?;
                self.envs.last_mut().unwrap().insert(name.value(), val);
                Ok(Value::Nil)
            }
            Stmt::Empty => Ok(Value::Nil),
        }
    }

    pub fn exec_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Print(expr) => {
                println!("{}", self.exec_expr(expr)?.str());
                Ok(Value::Nil)
            }
            Expr::Prompt(expr) => {
                print!("{}", self.exec_expr(expr)?.str());
                std::io::stdout().flush();
                let mut result = String::new();
                match std::io::stdin().read_line(&mut result) {
                    Ok(_) => {},
                    Err(e) => return Err(e.to_string()),
                }
                return Ok(Value::String(result))
            },
            Expr::Term(term) => self.exec_term(term),
            Expr::Bin { lhs, op, rhs } => match op.value().as_str() {
                "+" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                    (Value::String(a), b) => Ok(Value::String(format!("{a}{}", b.str()))),
                    (a, Value::String(b)) => Ok(Value::String(format!("{}{b}", a.str()))),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '+' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "-" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '-' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "/" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Float(*a as f64 / *b as f64)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / *b as f64)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '/' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "//" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Int(a / *b as i64)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Int(*a as i64 / b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Int(*a as i64 / *b as i64)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '//' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "%" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '%' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "*" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '*' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "??" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Nil, a) => Ok(a.clone()),
                    (a, _) => Ok(a.clone()),
                },

                ">" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '>' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                ">=" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '>=' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "<" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '<' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "<=" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '<=' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "&&" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a && *b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '&&' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "||" => match (&self.exec_expr(lhs)?, &self.exec_expr(rhs)?) {
                    (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a || *b)),
                    (a, b) => Err(format!(
                        "at {}:{} => unsupported '||' binary operator for type '{}' and '{}'",
                        op.row(),
                        op.col(),
                        a.type_name(),
                        b.type_name()
                    )),
                },
                "==" => Ok(Value::Bool(self.exec_expr(lhs)? == self.exec_expr(rhs)?)),
                "!=" => Ok(Value::Bool(self.exec_expr(lhs)? != self.exec_expr(rhs)?)),
                _ => unreachable!("BinExpr => exec_expr() => Interpreter"),
            },
            Expr::Unary { expr, op } => match (op.value().as_str(), self.exec_expr(expr)?) {
                ("?", Value::Nil) => Ok(Value::Bool(false)),
                ("?", _) => Ok(Value::Bool(true)),
                ("-", Value::Int(i)) => Ok(Value::Int(-i)),
                ("-", Value::Float(f)) => Ok(Value::Float(-f)),
                ("!", Value::Bool(b)) => Ok(Value::Bool(!b)),
                (_, e) => Err(format!(
                    "at {}:{} => unsupported '{}' unary operator for type '{}'",
                    op.row(),
                    op.col(),
                    op.value(),
                    e.type_name()
                )),
            },
            Expr::VariableAssignment { name, expr } => {
                let _ = self.get_var(&name.value())?;
                let val = self.exec_expr(expr)?;
                for env in self.envs.iter_mut().rev() {
                    if env.get(&name.value()).is_some() {
                        env.insert(name.value(), val.clone());
                        return Ok(val);
                    }
                }
                if self.builtin.contains_key(&name.value()) {
                    return Err(format!(
                        "at {}:{} => variable with name '{}' is builtin",
                        name.row(),
                        name.col(),
                        name.value()
                    ));
                }
                Err(format!(
                    "at {}:{} => variable with name '{}' is not defined",
                    name.row(),
                    name.col(),
                    name.value()
                ))
            },
            Expr::Block(blk) => {
                self.envs.push(env! {});
                let len = self.envs.len();
                let mut ret = Value::Nil;
                for stmt in blk.stmts.iter() {
                    ret = self.exec_stmt(stmt)?;
                    if self.envs.len() < len {
                        return Ok(ret);
                    }
                }
                self.envs.pop();
                Ok(ret)
            }
            Expr::ConditionalExpression {
                cond,
                body,
                else_block,
                loc,
            } => {
                self.envs.push(env! {});
                let mut ret = Value::Nil;
                let len = self.envs.len();

                match self.exec_expr(cond)? {
                    Value::Bool(b) => {
                        if b {
                            ret = self.exec_stmt(body)?;
                            if self.envs.len() < len {
                                return Ok(ret);
                            }
                        } else {
                            ret = self.exec_stmt(else_block)?;
                            if self.envs.len() < len {
                                return Ok(ret);
                            }
                        }
                    }
                    _ => {
                        return Err(format!(
                            "at {}:{} => non bool type in conditional expression",
                            loc.0, loc.1
                        ))
                    }
                }

                self.envs.pop();
                Ok(ret)
            }
            Expr::LoopExpression { cond, body, loc } => {
                self.envs.push(env! {});
                let mut ret = Value::Nil;

                while match self.exec_expr(cond)? {
                    Value::Bool(b) => b,
                    _ => {
                        return Err(format!(
                            "at {}:{} => non bool type in conditional expression",
                            loc.0, loc.1
                        ))
                    }
                } {
                    ret = self.exec_stmt(body)?;
                }

                self.envs.pop();
                Ok(ret)
            }
        }
    }

    pub fn exec_term(&mut self, term: &Term) -> Result<Value, String> {
        match term {
            Term::String(s) => Ok(Value::String(s.to_string())),
            Term::Float(f) => Ok(Value::Float(*f)),
            Term::Int(i) => Ok(Value::Int(*i)),
            Term::Bool(b) => Ok(Value::Bool(*b)),
            Term::Nil => Ok(Value::Nil),
            Term::Ident(id) => match self.get_var(&id.value()) {
                Ok(v) => Ok(v),
                Err(err) => Err(format!("at {}:{} => {err}", id.row(), id.col())),
            },
        }
    }

    pub fn get_var(&mut self, id: &str) -> Result<Value, String> {
        for env in self.envs.iter().rev() {
            if let Some(v) = env.get(id) {
                return Ok(v.clone());
            }
        }
        if let Some(v) = self.builtin.get(id) {
            return Ok(v.clone());
        }
        Err(format!("variable with name '{id}' is not defined"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

impl Value {
    pub fn str(&self) -> String {
        match self {
            Self::String(s) => s.clone(),
            Self::Int(i) => i.to_string(),
            Self::Float(f) => f.to_string(),
            Self::Bool(b) => b.to_string(),
            Self::Nil => "nil".to_string(),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Self::String(_) => "string".to_string(),
            Self::Int(_) => "int".to_string(),
            Self::Float(_) => "float".to_string(),
            Self::Bool(_) => "bool".to_string(),
            Self::Nil => "nil".to_string(),
        }
    }

    pub fn dbg_str(&self) -> String {
        match self {
            Self::String(s) => format!("\x1b[38;2;0;255;50m\"{s}\"\x1b[0m"),
            Self::Int(i) => format!("\x1b[38;2;255;0;255m{i}\x1b[0m"),
            Self::Float(f) => format!("\x1b[38;2;255;0;255m{f}F\x1b[0m"),
            Self::Bool(b) => format!("\x1b[38;2;255;100;100m{b}\x1b[0m"),
            Self::Nil => "\x1b[38;2;255;0;0mnil\x1b[0m".to_string(),
        }
    }
}
