/*
 * $NUM   := [1-9][0-9]*
 * $BOOL  := true | false
 * $ID    := string
 * $LIST  := '( $EXPRS )
 * $TUPLE := [ $EXPRS ]
 * $APPLY := ( $EXPRS )
 * $EXP   := $NUM | $BOOL | $ID | $LIST | $TUPLE | $APPLY
 * $EXPRS := $EXP $EXPRS | ∅
 */

use alloc::collections::linked_list::LinkedList;
use alloc::string::{String, ToString};

use super::Pos;

#[derive(Debug)]
pub struct SyntaxErr {
    pub pos: Pos,
    pub msg: &'static str,
}

pub struct Parser<'a> {
    pos: Pos,
    remain: &'a str,
}

#[derive(Debug)]
pub enum Expr {
    Num(i64, Pos),
    ID(String, Pos),
    Bool(bool, Pos),
    List(LinkedList<Expr>, Pos),
    Tuple(LinkedList<Expr>, Pos),
    Apply(LinkedList<Expr>, Pos),
}

impl Expr {
    pub fn get_pos(&self) -> Pos {
        match self {
            Expr::Num(_, pos) => *pos,
            Expr::ID(_, pos) => *pos,
            Expr::Bool(_, pos) => *pos,
            Expr::List(_, pos) => *pos,
            Expr::Tuple(_, pos) => *pos,
            Expr::Apply(_, pos) => *pos,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Parser<'a> {
        Parser {
            pos: Pos { line: 0, column: 0 },
            remain: code,
        }
    }

    pub fn parse(&mut self) -> Result<LinkedList<Expr>, SyntaxErr> {
        let mut exprs = LinkedList::new();

        loop {
            self.skip_spaces();
            if self.remain.len() == 0 {
                return Ok(exprs);
            }

            exprs.push_back(self.parse_expr()?);
        }
    }

    fn parse_id_bool(&mut self) -> Result<Expr, SyntaxErr> {
        let mut i = 0;

        for s in self.remain.chars() {
            if is_paren(s) || is_space(s) {
                break;
            }
            i += 1;
        }

        if i == 0 {
            Err(SyntaxErr {
                pos: self.pos,
                msg: "unexpected EOF",
            })
        } else {
            let c = self.remain[..i].to_string();
            self.remain = &self.remain[i..];
            let pos = self.pos;
            self.pos.column += i;

            if c == "true" {
                Ok(Expr::Bool(true, pos))
            } else if c == "false" {
                Ok(Expr::Bool(false, pos))
            } else {
                Ok(Expr::ID(c, pos))
            }
        }
    }

    fn parse_num(&mut self) -> Result<Expr, SyntaxErr> {
        let mut i = 0;

        let c = if self.remain.chars().nth(0) == Some('-') {
            i += 1;
            &self.remain[1..]
        } else {
            self.remain
        };

        for a in c.chars() {
            if '0' <= a && a <= '9' {
                i += 1;
            } else {
                break;
            }
        }

        let expr;

        match self.remain[0..i].parse::<i64>() {
            Ok(num) => {
                expr = Ok(Expr::Num(num, self.pos));
            }
            Err(_msg) => {
                return Err(SyntaxErr {
                    pos: self.pos,
                    msg: "failed to parse number",
                })
            }
        };

        self.pos.column += i;
        self.remain = &self.remain[i..];

        if self.remain.len() == 0 {
            return expr;
        }

        match self.remain.chars().nth(0) {
            Some(c0) => {
                if is_paren(c0) || is_space(c0) {
                    expr
                } else {
                    Err(SyntaxErr {
                        pos: self.pos,
                        msg: "expected '(', ')', '[', ']' or space",
                    })
                }
            }
            None => Err(SyntaxErr {
                pos: self.pos,
                msg: "unexpected EOF",
            }),
        }
    }

    fn skip_spaces(&mut self) {
        let mut i = 0;
        let mut prev = ' ';
        for s in self.remain.chars() {
            if is_space(s) {
                if s == '\r' || (s == '\n' && prev != '\r') {
                    self.pos.line += 1;
                    self.pos.column = 0;
                } else {
                    self.pos.column += 1;
                }
                i += 1;
                prev = s;
            } else {
                break;
            }
        }
        self.remain = &self.remain[i..]
    }

    fn parse_exprs(&mut self) -> Result<LinkedList<Expr>, SyntaxErr> {
        let mut exprs = LinkedList::<Expr>::new();
        self.skip_spaces();

        loop {
            self.skip_spaces();
            let c0 = self.remain.chars().nth(0);
            if self.remain.len() == 0 || c0 == Some(')') || c0 == Some(']') {
                break;
            }
            exprs.push_back(self.parse_expr()?);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, SyntaxErr> {
        self.skip_spaces();
        match self.remain.chars().nth(0) {
            Some('(') => self.parse_apply(),
            Some('\'') => self.parse_list(),
            Some('[') => self.parse_tuple(),
            Some(a) => {
                if '0' <= a && a <= '9' {
                    self.parse_num()
                } else if a == '-' {
                    match self.remain.chars().nth(1) {
                        Some(b) => {
                            if '0' <= b && b <= '9' {
                                self.parse_num()
                            } else {
                                self.parse_id_bool()
                            }
                        }
                        _ => self.parse_id_bool(),
                    }
                } else {
                    self.parse_id_bool()
                }
            }
            _ => Err(SyntaxErr {
                pos: self.pos,
                msg: "unexpected character",
            }),
        }
    }

    fn parse_apply(&mut self) -> Result<Expr, SyntaxErr> {
        self.remain = &self.remain[1..]; // skip '('
        let pos = self.pos;
        self.pos.column += 1;

        let exprs = self.parse_exprs()?;
        if self.remain.chars().nth(0) == Some(')') {
            self.remain = &self.remain[1..];
            self.pos.column += 1;
            Ok(Expr::Apply(exprs, pos))
        } else {
            Err(SyntaxErr {
                pos: self.pos,
                msg: "expected ')'",
            })
        }
    }

    fn parse_list(&mut self) -> Result<Expr, SyntaxErr> {
        let c = &self.remain[1..]; // skip '\''
        let pos = self.pos;
        self.pos.column += 1;

        match c.chars().nth(0) {
            Some('(') => {
                self.remain = &c[1..];
                let exprs = self.parse_exprs()?;
                if self.remain.chars().nth(0) == Some(')') {
                    self.remain = &self.remain[1..];
                    self.pos.column += 1;
                    Ok(Expr::List(exprs, pos))
                } else {
                    Err(SyntaxErr {
                        pos: self.pos,
                        msg: "expected ')'",
                    })
                }
            }
            _ => Err(SyntaxErr {
                pos: self.pos,
                msg: "expected '('",
            }),
        }
    }

    fn parse_tuple(&mut self) -> Result<Expr, SyntaxErr> {
        self.remain = &self.remain[1..]; // skip '['
        let pos = self.pos;
        self.pos.column += 1;

        let exprs = self.parse_exprs()?;
        if self.remain.chars().nth(0) == Some(']') {
            self.remain = &self.remain[1..];
            self.pos.column += 1;
            Ok(Expr::Tuple(exprs, pos))
        } else {
            Err(SyntaxErr {
                pos: self.pos,
                msg: "expected ']'",
            })
        }
    }
}

fn is_space(c: char) -> bool {
    c == ' ' || c == '\r' || c == '\n' || c == '\t'
}

fn is_paren(c: char) -> bool {
    c == '(' || c == ')' || c == '[' || c == ']'
}
