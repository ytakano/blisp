/*
 * $NUM   := [0-9]*
 * $HEX   := 0x[0-9a-fA-F]*
 * $OCT   := 0o[0-9a-fA-F]*
 * $BIN   := 0b[01]*
 * $BOOL  := true | false
 * $STR   := " string literal "
 * $CHAR  := ' character literal '
 * $ESCC  := character * $ID    := string
 * $LIST  := '( $EXPRS )
 * $TUPLE := [ $EXPRS ]
 * $APPLY := ( $EXPRS )
 * $EXP   := $HEX | $OCT | $BIN | $NUM | $BOOL | $ID | $LIST | $TUPLE | $APPLY
 * $EXPRS := $EXP $EXPRS | ∅
 */

use core::usize;

use alloc::collections::linked_list::LinkedList;
use alloc::string::{String, ToString};
use num_bigint::BigInt;
use num_traits::Zero;

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
    Str(String, Pos),
    Char(char, Pos),
    Num(BigInt, Pos),
    ID(String, Pos),
    Bool(bool, Pos),
    List(LinkedList<Expr>, Pos),
    Tuple(LinkedList<Expr>, Pos),
    Apply(LinkedList<Expr>, Pos),
}

impl Expr {
    pub fn get_pos(&self) -> Pos {
        match self {
            Expr::Char(_, pos) => *pos,
            Expr::Num(_, pos) => *pos,
            Expr::ID(_, pos) => *pos,
            Expr::Bool(_, pos) => *pos,
            Expr::List(_, pos) => *pos,
            Expr::Tuple(_, pos) => *pos,
            Expr::Apply(_, pos) => *pos,
            Expr::Str(_, pos) => *pos,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str, file_id: usize) -> Parser<'a> {
        Parser {
            pos: Pos {
                file_id,
                line: 0,
                column: 0,
            },
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
            if is_paren(s) || is_space(s) || s == ';' {
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

    fn parse_oct(&mut self) -> Result<Expr, SyntaxErr> {
        let mut n = Zero::zero();
        let mut i = 0;

        for c in self.remain.chars() {
            let m = if '0' <= c && c <= '7' {
                c as u32 - '0' as u32
            } else {
                break;
            };
            n *= 8;
            n += m;
            i += 1;
        }

        if i == 0 {
            return Err(SyntaxErr {
                pos: self.pos,
                msg: "expect hexadecimal number",
            });
        }

        let expr = Expr::Num(n, self.pos);

        self.pos.column += i;
        self.remain = &self.remain[i..];

        self.check_eof(expr)
    }

    fn parse_hex(&mut self) -> Result<Expr, SyntaxErr> {
        let mut n = Zero::zero();
        let mut i = 0;

        for c in self.remain.chars() {
            let m = if '0' <= c && c <= '9' {
                c as u32 - '0' as u32
            } else if 'a' <= c && c <= 'f' {
                c as u32 - 'a' as u32 + 10
            } else if 'A' <= c && c <= 'F' {
                c as u32 - 'A' as u32 + 10
            } else {
                break;
            };
            n *= 16;
            n += m;
            i += 1;
        }

        if i == 0 {
            return Err(SyntaxErr {
                pos: self.pos,
                msg: "expect hexadecimal number",
            });
        }

        let expr = Expr::Num(n, self.pos);

        self.pos.column += i;
        self.remain = &self.remain[i..];

        self.check_eof(expr)
    }

    fn check_eof(&self, expr: Expr) -> Result<Expr, SyntaxErr> {
        if self.remain.len() == 0 {
            return Ok(expr);
        }

        match self.remain.chars().nth(0) {
            Some(c0) => {
                if is_paren(c0) || is_space(c0) {
                    Ok(expr)
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

    fn parse_binary(&mut self) -> Result<Expr, SyntaxErr> {
        let mut n = Zero::zero();
        let mut i = 0;

        for c in self.remain.chars() {
            let m = if c == '0' {
                0
            } else if c == '1' {
                1
            } else {
                break;
            };
            n *= 2;
            n += m;
            i += 1;
        }

        if i == 0 {
            return Err(SyntaxErr {
                pos: self.pos,
                msg: "expect binary number",
            });
        }

        let expr = Expr::Num(n, self.pos);

        self.pos.column += i;
        self.remain = &self.remain[i..];

        self.check_eof(expr)
    }

    fn parse_num(&mut self) -> Result<Expr, SyntaxErr> {
        let mut i = 0;
        let is_minus;

        let mut cs = self.remain.chars();
        let c0 = cs.nth(0);
        let c1 = cs.nth(0);
        let c = match (c0, c1) {
            (Some('-'), _) => {
                is_minus = true;
                i += 1;
                &self.remain[1..]
            }
            (Some('0'), Some('x')) => {
                self.pos.column += 2;
                self.remain = &self.remain[2..];
                return self.parse_hex();
            }
            (Some('0'), Some('b')) => {
                self.pos.column += 2;
                self.remain = &self.remain[2..];
                return self.parse_binary();
            }
            (Some('0'), Some('o')) => {
                self.pos.column += 2;
                self.remain = &self.remain[2..];
                return self.parse_oct();
            }
            _ => {
                is_minus = false;
                self.remain
            }
        };

        // parse decimal number
        let mut n = Zero::zero();

        for a in c.chars() {
            if '0' <= a && a <= '9' {
                n *= 10;
                n += a as usize - '0' as usize;
                i += 1;
            } else {
                break;
            }
        }

        if is_minus {
            n *= -1;
        }

        let expr = Expr::Num(n, self.pos);

        self.pos.column += i;
        self.remain = &self.remain[i..];

        self.check_eof(expr)
    }

    fn skip_spaces(&mut self) {
        let mut i = 0;
        let mut prev = ' ';
        let mut is_comment = false;
        for s in self.remain.chars() {
            if is_comment {
                if s == '\r' || s == '\n' {
                    is_comment = false;
                } else {
                    self.pos.column += 1;
                    i += 1;
                    prev = s;
                    continue;
                }
            }

            if s == ';' {
                is_comment = true;
                self.pos.column += 1;
            } else if is_space(s) {
                if s == '\r' || (s == '\n' && prev != '\r') {
                    self.pos.line += 1;
                    self.pos.column = 0;
                } else {
                    self.pos.column += 1;
                }
            } else {
                break;
            }
            i += 1;
            prev = s;
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
            Some('"') => self.parse_string(),
            Some('`') => self.parse_char(),
            Some(a) => {
                if a == ')' {
                    Err(SyntaxErr {
                        pos: self.pos,
                        msg: "invalid )",
                    })
                } else if '0' <= a && a <= '9' {
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

    fn parse_char(&mut self) -> Result<Expr, SyntaxErr> {
        self.remain = &self.remain[1..]; // skip '`'
        let pos = self.pos;
        self.pos.column += 1;

        if let Some(c) = self.remain.chars().nth(0) {
            match c {
                '\\' => {
                    if let Some(c1) = self.remain.chars().nth(1) {
                        // TODO:
                        //  \x41     | 7-bit character code (exactly 2 digits, up to 0x7F)
                        //  \u{7FFF} | 24-bit Unicode character code (up to 6 digits)

                        let esc = match c1 {
                            'r' => '\r',
                            'n' => '\n',
                            't' => '\t',
                            '0' => '\0',
                            '\\' => '\\',
                            '`' => '`',
                            _ => {
                                return Err(SyntaxErr {
                                    pos: self.pos,
                                    msg: "invalid escape character",
                                });
                            }
                        };

                        if let Some('`') = self.remain.chars().nth(2) {
                            self.remain = &self.remain[3..];
                            self.pos.column += 3;
                            Ok(Expr::Char(esc, pos))
                        } else {
                            self.pos.column += 1;
                            Err(SyntaxErr {
                                pos: self.pos,
                                msg: "expected `",
                            })
                        }
                    } else {
                        return Err(SyntaxErr {
                            pos: self.pos,
                            msg: "expected escape character",
                        });
                    }
                }
                '\r' | '\n' => Err(SyntaxErr {
                    pos: self.pos,
                    msg: "use \\r or \\n",
                }),
                c => {
                    if let Some('`') = self.remain.chars().nth(1) {
                        self.remain = &self.remain[2..];
                        self.pos.column += 2;
                        Ok(Expr::Char(c, pos))
                    } else {
                        self.pos.column += 1;
                        Err(SyntaxErr {
                            pos: self.pos,
                            msg: "expected `",
                        })
                    }
                }
            }
        } else {
            Err(SyntaxErr {
                pos: self.pos,
                msg: "expected character literal",
            })
        }
    }

    fn parse_string(&mut self) -> Result<Expr, SyntaxErr> {
        self.remain = &self.remain[1..]; // skip '"'
        let pos = self.pos;
        self.pos.column += 1;

        let mut prev = ' ';
        let mut str = "".to_string();
        loop {
            if let Some(c) = self.remain.chars().nth(0) {
                match c {
                    '"' => {
                        self.pos.column += 1;
                        self.remain = &self.remain[1..];
                        break;
                    }
                    '\\' => {
                        if let Some(c1) = self.remain.chars().nth(1) {
                            // TODO:
                            //  \x41     | 7-bit character code (exactly 2 digits, up to 0x7F)
                            //  \u{7FFF} | 24-bit Unicode character code (up to 6 digits)

                            let esc = match c1 {
                                'r' => '\r',
                                'n' => '\n',
                                't' => '\t',
                                '0' => '\0',
                                '\\' => '\\',
                                '"' => '"',
                                _ => {
                                    return Err(SyntaxErr {
                                        pos: self.pos,
                                        msg: "invalid escape character",
                                    });
                                }
                            };

                            str.push(esc);
                            self.remain = &self.remain[2..];
                            self.pos.column += 2;
                            continue;
                        } else {
                            return Err(SyntaxErr {
                                pos: self.pos,
                                msg: "expected escape character",
                            });
                        }
                    }
                    _ => {
                        if c == '\r' || (c == '\n' && prev != '\r') {
                            self.pos.line += 1;
                            self.pos.column = 0;
                        } else {
                            self.pos.column += 1;
                        }

                        prev = c;
                        str.push(c);
                        self.remain = &self.remain[1..];
                    }
                }
            }
        }

        Ok(Expr::Str(str, pos))
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
