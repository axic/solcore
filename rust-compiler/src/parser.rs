// Recursive-descent SAIL parser. Produces a `CompUnit` AST from tokens.

use crate::ast::*;
use crate::lexer::{Tok, Token};
use std::fmt;

pub struct Parser {
    toks: Vec<Token>,
    pos: usize,
}

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at {}:{}: {}", self.line, self.col, self.msg)
    }
}

type R<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(toks: Vec<Token>) -> Self { Parser { toks, pos: 0 } }

    fn peek(&self) -> &Tok { &self.toks[self.pos].tok }
    fn peek_at(&self, n: usize) -> &Tok { &self.toks[self.pos + n].tok }
    fn cur(&self) -> &Token { &self.toks[self.pos] }
    fn bump(&mut self) -> Token { let t = self.toks[self.pos].clone(); self.pos += 1; t }
    fn err<T>(&self, msg: impl Into<String>) -> R<T> {
        let c = self.cur();
        Err(ParseError { msg: msg.into(), line: c.line, col: c.col })
    }

    fn expect(&mut self, t: Tok) -> R<()> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            self.bump();
            Ok(())
        } else {
            let got = format!("{:?}", self.peek());
            self.err(format!("expected {:?}, found {}", t, got))
        }
    }

    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(t) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect_ident(&mut self) -> R<String> {
        match self.peek().clone() {
            Tok::Ident(s) => { self.bump(); Ok(s) }
            other => self.err(format!("expected identifier, found {:?}", other)),
        }
    }

    // ----- Top-level: CompUnit -----------------------------------------------

    pub fn parse_unit(&mut self) -> R<CompUnit> {
        let mut imports = Vec::new();
        while matches!(self.peek(), Tok::Import) {
            imports.push(self.parse_import()?);
        }
        let mut decls = Vec::new();
        while !matches!(self.peek(), Tok::Eof) {
            decls.push(self.parse_top_decl()?);
        }
        Ok(CompUnit { imports, decls })
    }

    fn parse_import(&mut self) -> R<Import> {
        self.expect(Tok::Import)?;
        let mut external = false;
        let mut from_lib = false;
        if self.eat(&Tok::At) {
            external = true;
            // consume "package."
            let _pkg = self.expect_ident()?;
            self.expect(Tok::Dot)?;
        }
        // first segment
        let mut path = vec![self.expect_ident()?];
        if path[0] == "lib" && matches!(self.peek(), Tok::Dot) {
            from_lib = true;
            self.bump();
            path.push(self.expect_ident()?);
        }
        while matches!(self.peek(), Tok::Dot)
            && matches!(self.peek_at(1), Tok::Ident(_))
        {
            self.bump();
            path.push(self.expect_ident()?);
        }
        // Now we either have `;`, `as Alias`, or `.{ items }` (selective)
        let kind = if self.eat(&Tok::As) {
            let alias = self.expect_ident()?;
            self.expect(Tok::Semi)?;
            ImportKind::Whole(Some(alias))
        } else if matches!(self.peek(), Tok::Dot)
            && matches!(self.peek_at(1), Tok::LBrace)
        {
            self.bump(); // .
            self.bump(); // {
            let mut items = Vec::new();
            if !matches!(self.peek(), Tok::RBrace) {
                items.push(self.parse_import_item()?);
                while self.eat(&Tok::Comma) {
                    if matches!(self.peek(), Tok::RBrace) { break; }
                    items.push(self.parse_import_item()?);
                }
            }
            self.expect(Tok::RBrace)?;
            let mut hiding = Vec::new();
            if self.eat(&Tok::Hiding) {
                self.expect(Tok::LBrace)?;
                if !matches!(self.peek(), Tok::RBrace) {
                    hiding.push(self.expect_ident()?);
                    while self.eat(&Tok::Comma) {
                        if matches!(self.peek(), Tok::RBrace) { break; }
                        hiding.push(self.expect_ident()?);
                    }
                }
                self.expect(Tok::RBrace)?;
            }
            self.expect(Tok::Semi)?;
            ImportKind::Select(items, hiding)
        } else {
            self.expect(Tok::Semi)?;
            ImportKind::Whole(None)
        };
        Ok(Import { path, external, from_lib, kind })
    }

    fn parse_import_item(&mut self) -> R<ImportItem> {
        if self.eat(&Tok::Star) { return Ok(ImportItem::Wildcard); }
        let n = self.expect_ident()?;
        if self.eat(&Tok::LParen) {
            if self.eat(&Tok::Star) {
                self.expect(Tok::RParen)?;
                return Ok(ImportItem::NameStar(n));
            }
            let mut subs = Vec::new();
            if !matches!(self.peek(), Tok::RParen) {
                subs.push(self.expect_ident()?);
                while self.eat(&Tok::Comma) {
                    subs.push(self.expect_ident()?);
                }
            }
            self.expect(Tok::RParen)?;
            return Ok(ImportItem::NameList(n, subs));
        }
        Ok(ImportItem::Name(n))
    }

    // ----- Top declarations --------------------------------------------------

    fn parse_top_decl(&mut self) -> R<TopDecl> {
        match self.peek() {
            Tok::Pragma => self.parse_pragma().map(TopDecl::Pragma),
            Tok::Export => self.parse_export().map(TopDecl::Export),
            Tok::Contract => self.parse_contract().map(TopDecl::Contract),
            Tok::Data => self.parse_data().map(TopDecl::Data),
            Tok::Type => self.parse_type_synonym().map(TopDecl::TypeSyn),
            Tok::Forall => {
                let prefix = self.parse_sig_prefix()?;
                self.parse_top_decl_after_prefix(Some(prefix))
            }
            Tok::Default => self.parse_top_decl_after_prefix(None),
            Tok::Class => self.parse_class(None).map(TopDecl::Class),
            Tok::Instance => self.parse_instance(None, false).map(TopDecl::Instance),
            Tok::Function => self.parse_function(None).map(TopDecl::Func),
            _ => self.err(format!("unexpected top-level token {:?}", self.peek())),
        }
    }

    fn parse_top_decl_after_prefix(&mut self, prefix: Option<SigPrefix>) -> R<TopDecl> {
        match self.peek() {
            Tok::Class => self.parse_class(prefix).map(TopDecl::Class),
            Tok::Instance => self.parse_instance(prefix, false).map(TopDecl::Instance),
            Tok::Default => {
                self.bump();
                self.parse_instance(prefix, true).map(TopDecl::Instance)
            }
            Tok::Function => self.parse_function(prefix).map(TopDecl::Func),
            other => self.err(format!("expected function/class/instance after forall, found {:?}", other)),
        }
    }

    fn parse_pragma(&mut self) -> R<Pragma> {
        self.expect(Tok::Pragma)?;
        // The kind is an identifier with optional `-` separators (e.g. no-coverage-condition)
        let mut kind = self.expect_ident()?;
        while self.eat(&Tok::Minus) {
            kind.push('-');
            kind.push_str(&self.expect_ident()?);
        }
        let mut targets = Vec::new();
        if !matches!(self.peek(), Tok::Semi) {
            targets.push(self.expect_ident()?);
            while self.eat(&Tok::Comma) {
                targets.push(self.expect_ident()?);
            }
        }
        self.expect(Tok::Semi)?;
        Ok(Pragma { kind, targets })
    }

    fn parse_export(&mut self) -> R<ExportDecl> {
        self.expect(Tok::Export)?;
        // Optional "from <module>"
        let from = if matches!(self.peek(), Tok::From) {
            self.bump();
            let mut p = vec![self.expect_ident()?];
            while self.eat(&Tok::Dot) { p.push(self.expect_ident()?); }
            Some(p)
        } else {
            None
        };
        self.expect(Tok::LBrace)?;
        let mut items = Vec::new();
        if !matches!(self.peek(), Tok::RBrace) {
            items.push(self.parse_export_item()?);
            while self.eat(&Tok::Comma) {
                if matches!(self.peek(), Tok::RBrace) { break; }
                items.push(self.parse_export_item()?);
            }
        }
        self.expect(Tok::RBrace)?;
        self.expect(Tok::Semi)?;
        Ok(ExportDecl { items, from })
    }

    fn parse_export_item(&mut self) -> R<ExportItem> {
        if self.eat(&Tok::Star) { return Ok(ExportItem::Wildcard); }
        let n = self.expect_ident()?;
        if self.eat(&Tok::LParen) {
            if self.eat(&Tok::Star) {
                self.expect(Tok::RParen)?;
                return Ok(ExportItem::NameStar(n));
            }
            let mut subs = Vec::new();
            if !matches!(self.peek(), Tok::RParen) {
                subs.push(self.expect_ident()?);
                while self.eat(&Tok::Comma) {
                    subs.push(self.expect_ident()?);
                }
            }
            self.expect(Tok::RParen)?;
            return Ok(ExportItem::NameList(n, subs));
        }
        Ok(ExportItem::Name(n))
    }

    // ----- Pragmas / forall prefix -------------------------------------------

    fn parse_sig_prefix(&mut self) -> R<SigPrefix> {
        self.expect(Tok::Forall)?;
        let mut vars = Vec::new();
        while let Tok::Ident(_) = self.peek() {
            vars.push(self.expect_ident()?);
        }
        self.expect(Tok::Dot)?;
        // Optional constraints terminated by `=>`
        let mut constraints = Vec::new();
        // We need to determine whether a constraint list follows. A constraint
        // looks like Type ":" ClassName. After the `forall a b .` we either
        // see constraints or directly the declaration token (function/class/...).
        if !matches!(self.peek(), Tok::Function | Tok::Class | Tok::Instance | Tok::Default) {
            constraints.push(self.parse_constraint()?);
            while self.eat(&Tok::Comma) {
                constraints.push(self.parse_constraint()?);
            }
            self.expect(Tok::FatArrow)?;
        }
        Ok(SigPrefix { vars, constraints })
    }

    fn parse_constraint(&mut self) -> R<Constraint> {
        let ty = self.parse_type()?;
        self.expect(Tok::Colon)?;
        let class_name = self.parse_qualified_name()?;
        let mut args = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                args.push(self.parse_type()?);
                while self.eat(&Tok::Comma) {
                    args.push(self.parse_type()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        Ok(Constraint { ty, class_name, args })
    }

    fn parse_qualified_name(&mut self) -> R<Vec<String>> {
        let mut out = vec![self.expect_ident()?];
        while matches!(self.peek(), Tok::Dot)
            && matches!(self.peek_at(1), Tok::Ident(_))
        {
            self.bump();
            out.push(self.expect_ident()?);
        }
        Ok(out)
    }

    // ----- Types -------------------------------------------------------------

    pub fn parse_type(&mut self) -> R<Type> {
        // `@T` proxy
        if self.eat(&Tok::At) {
            let inner = self.parse_type()?;
            return Ok(Type::Proxy(Box::new(inner)));
        }
        // `(...)` either tuple type or function arg list
        if matches!(self.peek(), Tok::LParen) {
            self.bump();
            let mut items: Vec<Type> = Vec::new();
            if !matches!(self.peek(), Tok::RParen) {
                items.push(self.parse_type()?);
                while self.eat(&Tok::Comma) {
                    items.push(self.parse_type()?);
                }
            }
            self.expect(Tok::RParen)?;
            if self.eat(&Tok::Arrow) {
                let ret = self.parse_type()?;
                return Ok(Type::Fun(items, Box::new(ret)));
            }
            return Ok(Type::Tuple(items));
        }
        // Named: qualified ident, optional type args
        let name = self.parse_qualified_name()?;
        let mut args = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                args.push(self.parse_type()?);
                while self.eat(&Tok::Comma) {
                    args.push(self.parse_type()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        Ok(Type::Named(name, args))
    }

    // ----- Data declarations -------------------------------------------------

    fn parse_data(&mut self) -> R<DataDef> {
        self.expect(Tok::Data)?;
        let name = self.expect_ident()?;
        let mut params = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                params.push(self.expect_ident()?);
                while self.eat(&Tok::Comma) {
                    params.push(self.expect_ident()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        let mut ctors = Vec::new();
        if self.eat(&Tok::Eq) {
            ctors.push(self.parse_data_ctor()?);
            while self.eat(&Tok::Pipe) {
                ctors.push(self.parse_data_ctor()?);
            }
        }
        self.expect(Tok::Semi)?;
        Ok(DataDef { name, params, ctors })
    }

    fn parse_data_ctor(&mut self) -> R<DataCtor> {
        let name = self.expect_ident()?;
        let mut fields = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                fields.push(self.parse_type()?);
                while self.eat(&Tok::Comma) {
                    fields.push(self.parse_type()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        Ok(DataCtor { name, fields })
    }

    fn parse_type_synonym(&mut self) -> R<TypeSynonym> {
        self.expect(Tok::Type)?;
        let name = self.expect_ident()?;
        let mut params = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                params.push(self.expect_ident()?);
                while self.eat(&Tok::Comma) {
                    params.push(self.expect_ident()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        self.expect(Tok::Eq)?;
        let rhs = self.parse_type()?;
        self.expect(Tok::Semi)?;
        Ok(TypeSynonym { name, params, rhs })
    }

    // ----- Class / Instance --------------------------------------------------

    fn parse_class(&mut self, prefix: Option<SigPrefix>) -> R<ClassDef> {
        self.expect(Tok::Class)?;
        let self_var = self.expect_ident()?;
        self.expect(Tok::Colon)?;
        let class_name = self.expect_ident()?;
        let mut aux_args = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                aux_args.push(self.expect_ident()?);
                while self.eat(&Tok::Comma) {
                    aux_args.push(self.expect_ident()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        self.expect(Tok::LBrace)?;
        let mut methods = Vec::new();
        while !matches!(self.peek(), Tok::RBrace) {
            // Methods inside classes may themselves have a forall prefix
            let inner_prefix = if matches!(self.peek(), Tok::Forall) {
                Some(self.parse_sig_prefix()?)
            } else {
                None
            };
            self.expect(Tok::Function)?;
            let sig = self.parse_signature(inner_prefix)?;
            self.expect(Tok::Semi)?;
            methods.push(sig);
        }
        self.expect(Tok::RBrace)?;
        Ok(ClassDef { prefix, self_var, class_name, aux_args, methods })
    }

    fn parse_instance(&mut self, prefix: Option<SigPrefix>, is_default: bool) -> R<InstDef> {
        if !is_default && self.eat(&Tok::Default) {
            return self.parse_instance(prefix, true);
        }
        self.expect(Tok::Instance)?;
        let head_ty = self.parse_type()?;
        self.expect(Tok::Colon)?;
        let class_name = self.parse_qualified_name()?;
        let mut class_args = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                class_args.push(self.parse_type()?);
                while self.eat(&Tok::Comma) {
                    class_args.push(self.parse_type()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        self.expect(Tok::LBrace)?;
        let mut methods = Vec::new();
        while !matches!(self.peek(), Tok::RBrace) {
            let inner_prefix = if matches!(self.peek(), Tok::Forall) {
                Some(self.parse_sig_prefix()?)
            } else {
                None
            };
            methods.push(self.parse_function(inner_prefix)?);
        }
        self.expect(Tok::RBrace)?;
        Ok(InstDef { prefix, is_default, head_ty, class_name, class_args, methods })
    }

    // ----- Functions / signatures --------------------------------------------

    fn parse_signature(&mut self, prefix: Option<SigPrefix>) -> R<Signature> {
        let name = self.expect_ident()?;
        self.expect(Tok::LParen)?;
        let mut params = Vec::new();
        if !matches!(self.peek(), Tok::RParen) {
            params.push(self.parse_param()?);
            while self.eat(&Tok::Comma) {
                params.push(self.parse_param()?);
            }
        }
        self.expect(Tok::RParen)?;
        let ret = if self.eat(&Tok::Arrow) { Some(self.parse_type()?) } else { None };
        Ok(Signature { prefix, name, params, ret })
    }

    fn parse_param(&mut self) -> R<Param> {
        let name = self.expect_ident()?;
        let ty = if self.eat(&Tok::Colon) { Some(self.parse_type()?) } else { None };
        Ok(Param { name, ty })
    }

    fn parse_function(&mut self, prefix: Option<SigPrefix>) -> R<Function> {
        self.expect(Tok::Function)?;
        let sig = self.parse_signature(prefix)?;
        // `function f() -> T { ... }` or short form `= expr;` or signature-only `;`.
        if self.eat(&Tok::Eq) {
            let e = self.parse_expr()?;
            self.expect(Tok::Semi)?;
            return Ok(Function { sig, body: None, short: Some(e) });
        }
        if self.eat(&Tok::Semi) {
            return Ok(Function { sig, body: None, short: None });
        }
        let body = self.parse_body()?;
        Ok(Function { sig, body: Some(body), short: None })
    }

    fn parse_body(&mut self) -> R<Body> {
        self.expect(Tok::LBrace)?;
        let mut stmts = Vec::new();
        while !matches!(self.peek(), Tok::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(Tok::RBrace)?;
        Ok(Body { stmts })
    }

    // ----- Statements --------------------------------------------------------

    fn parse_stmt(&mut self) -> R<Stmt> {
        match self.peek() {
            Tok::Let => {
                self.bump();
                let name = self.expect_ident()?;
                let ty = if self.eat(&Tok::Colon) { Some(self.parse_type()?) } else { None };
                let init = if self.eat(&Tok::Eq) { Some(self.parse_expr()?) } else { None };
                self.expect(Tok::Semi)?;
                Ok(Stmt::Let(name, ty, init))
            }
            Tok::Return => {
                self.bump();
                if self.eat(&Tok::Semi) { return Ok(Stmt::Return(None)); }
                let e = self.parse_expr()?;
                self.expect(Tok::Semi)?;
                Ok(Stmt::Return(Some(e)))
            }
            Tok::Match => {
                self.bump();
                let mut args = vec![self.parse_expr()?];
                while self.eat(&Tok::Comma) {
                    args.push(self.parse_expr()?);
                }
                self.expect(Tok::LBrace)?;
                let mut eqs = Vec::new();
                while matches!(self.peek(), Tok::Pipe) {
                    self.bump();
                    eqs.push(self.parse_equation()?);
                }
                self.expect(Tok::RBrace)?;
                Ok(Stmt::Match(args, eqs))
            }
            Tok::If => {
                self.bump();
                self.expect(Tok::LParen)?;
                let c = self.parse_expr()?;
                self.expect(Tok::RParen)?;
                let then_b = self.parse_body()?;
                let else_b = if self.eat(&Tok::Else) { Some(self.parse_body()?) } else { None };
                Ok(Stmt::If(c, then_b, else_b))
            }
            Tok::Assembly => {
                self.bump();
                let blk = self.parse_asm_block()?;
                // optional trailing semicolon
                self.eat(&Tok::Semi);
                Ok(Stmt::Asm(blk))
            }
            _ => {
                // Expression statement OR assignment.
                let e = self.parse_expr()?;
                if self.eat(&Tok::Eq) {
                    let r = self.parse_expr()?;
                    self.expect(Tok::Semi)?;
                    return Ok(Stmt::Assign(e, r));
                }
                if self.eat(&Tok::PlusEq) {
                    let r = self.parse_expr()?;
                    self.expect(Tok::Semi)?;
                    return Ok(Stmt::AddAssign(e, r));
                }
                if self.eat(&Tok::MinusEq) {
                    let r = self.parse_expr()?;
                    self.expect(Tok::Semi)?;
                    return Ok(Stmt::SubAssign(e, r));
                }
                // Implicit-return form: trailing expression with no `;` before `}`.
                if matches!(self.peek(), Tok::RBrace) {
                    return Ok(Stmt::Return(Some(e)));
                }
                self.expect(Tok::Semi)?;
                Ok(Stmt::Expr(e))
            }
        }
    }

    fn parse_equation(&mut self) -> R<Equation> {
        // Patterns until `=>`. Patterns are comma-separated.
        let mut patterns = vec![self.parse_pattern()?];
        while self.eat(&Tok::Comma) {
            patterns.push(self.parse_pattern()?);
        }
        self.expect(Tok::FatArrow)?;
        // Statements until next `|` or `}`.
        let mut stmts = Vec::new();
        while !matches!(self.peek(), Tok::Pipe | Tok::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(Equation { patterns, body: Body { stmts } })
    }

    // ----- Patterns ----------------------------------------------------------

    fn parse_pattern(&mut self) -> R<Pattern> {
        match self.peek().clone() {
            Tok::Underscore => { self.bump(); Ok(Pattern::Wildcard) }
            Tok::Int(s) => { self.bump(); Ok(Pattern::Int(s)) }
            Tok::Str(s) => { self.bump(); Ok(Pattern::Str(s)) }
            Tok::True => { self.bump(); Ok(Pattern::Bool(true)) }
            Tok::False => { self.bump(); Ok(Pattern::Bool(false)) }
            Tok::Dot => {
                self.bump();
                let n = self.expect_ident()?;
                let mut args = Vec::new();
                if self.eat(&Tok::LParen) {
                    if !matches!(self.peek(), Tok::RParen) {
                        args.push(self.parse_pattern()?);
                        while self.eat(&Tok::Comma) {
                            args.push(self.parse_pattern()?);
                        }
                    }
                    self.expect(Tok::RParen)?;
                }
                Ok(Pattern::DotConstr(n, args))
            }
            Tok::LParen => {
                self.bump();
                if self.eat(&Tok::RParen) { return Ok(Pattern::Tuple(Vec::new())); }
                let mut items = vec![self.parse_pattern()?];
                while self.eat(&Tok::Comma) { items.push(self.parse_pattern()?); }
                self.expect(Tok::RParen)?;
                if items.len() == 1 {
                    Ok(items.into_iter().next().unwrap())
                } else {
                    Ok(Pattern::Tuple(items))
                }
            }
            Tok::Ident(_) => {
                let name = self.parse_qualified_name()?;
                let has_args = matches!(self.peek(), Tok::LParen);
                if !has_args && name.len() == 1 {
                    return Ok(Pattern::Var(name.into_iter().next().unwrap()));
                }
                let mut args = Vec::new();
                if self.eat(&Tok::LParen) {
                    if !matches!(self.peek(), Tok::RParen) {
                        args.push(self.parse_pattern()?);
                        while self.eat(&Tok::Comma) {
                            args.push(self.parse_pattern()?);
                        }
                    }
                    self.expect(Tok::RParen)?;
                }
                Ok(Pattern::Constr(name, args))
            }
            other => self.err(format!("expected pattern, found {:?}", other)),
        }
    }

    // ----- Expressions -------------------------------------------------------

    pub fn parse_expr(&mut self) -> R<Expr> {
        // Optional `if expr then expr else expr` form is not used in std.solc.
        // We parse the binary operator chain.
        self.parse_or()
    }

    fn parse_or(&mut self) -> R<Expr> {
        let mut lhs = self.parse_and()?;
        while self.eat(&Tok::OrOr) {
            let rhs = self.parse_and()?;
            lhs = Expr::BinOp(BinOp::Or, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_and(&mut self) -> R<Expr> {
        let mut lhs = self.parse_cmp()?;
        while self.eat(&Tok::AndAnd) {
            let rhs = self.parse_cmp()?;
            lhs = Expr::BinOp(BinOp::And, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_cmp(&mut self) -> R<Expr> {
        let mut lhs = self.parse_add()?;
        loop {
            let op = match self.peek() {
                Tok::Lt => BinOp::Lt,
                Tok::Gt => BinOp::Gt,
                Tok::LtEq => BinOp::Le,
                Tok::GtEq => BinOp::Ge,
                Tok::EqEq => BinOp::Eq,
                Tok::NotEq => BinOp::Ne,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_add()?;
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_add(&mut self) -> R<Expr> {
        let mut lhs = self.parse_mul()?;
        loop {
            let op = match self.peek() {
                Tok::Plus => BinOp::Add,
                Tok::Minus => BinOp::Sub,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_mul()?;
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> R<Expr> {
        let mut lhs = self.parse_unary()?;
        loop {
            let op = match self.peek() {
                Tok::Star => BinOp::Mul,
                Tok::Slash => BinOp::Div,
                Tok::Percent => BinOp::Mod,
                _ => break,
            };
            self.bump();
            let rhs = self.parse_unary()?;
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> R<Expr> {
        if self.eat(&Tok::Bang) {
            let e = self.parse_unary()?;
            return Ok(Expr::UnOp(UnOp::Not, Box::new(e)));
        }
        if self.eat(&Tok::Minus) {
            let e = self.parse_unary()?;
            return Ok(Expr::UnOp(UnOp::Neg, Box::new(e)));
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> R<Expr> {
        let mut e = self.parse_primary()?;
        loop {
            match self.peek() {
                Tok::LParen => {
                    self.bump();
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Tok::RParen) {
                        args.push(self.parse_expr()?);
                        while self.eat(&Tok::Comma) {
                            args.push(self.parse_expr()?);
                        }
                    }
                    self.expect(Tok::RParen)?;
                    e = Expr::Call(Box::new(e), args);
                }
                Tok::LBracket => {
                    self.bump();
                    let i = self.parse_expr()?;
                    self.expect(Tok::RBracket)?;
                    e = Expr::Index(Box::new(e), Box::new(i));
                }
                Tok::Colon => {
                    self.bump();
                    let t = self.parse_type()?;
                    e = Expr::Annot(Box::new(e), t);
                }
                _ => break,
            }
        }
        Ok(e)
    }

    fn parse_primary(&mut self) -> R<Expr> {
        match self.peek().clone() {
            Tok::Int(s) => { self.bump(); Ok(Expr::Int(s)) }
            Tok::Str(s) => { self.bump(); Ok(Expr::Str(s)) }
            Tok::True => { self.bump(); Ok(Expr::Bool(true)) }
            Tok::False => { self.bump(); Ok(Expr::Bool(false)) }
            Tok::Dot => {
                self.bump();
                let n = self.expect_ident()?;
                let mut args = Vec::new();
                if self.eat(&Tok::LParen) {
                    if !matches!(self.peek(), Tok::RParen) {
                        args.push(self.parse_expr()?);
                        while self.eat(&Tok::Comma) {
                            args.push(self.parse_expr()?);
                        }
                    }
                    self.expect(Tok::RParen)?;
                }
                Ok(Expr::DotCtor(n, args))
            }
            Tok::At => {
                self.bump();
                let t = self.parse_type()?;
                Ok(Expr::ProxyTy(t))
            }
            Tok::LParen => {
                self.bump();
                if self.eat(&Tok::RParen) {
                    return Ok(Expr::Tuple(Vec::new()));
                }
                let mut items = vec![self.parse_expr()?];
                while self.eat(&Tok::Comma) {
                    items.push(self.parse_expr()?);
                }
                self.expect(Tok::RParen)?;
                if items.len() == 1 {
                    Ok(items.into_iter().next().unwrap())
                } else {
                    Ok(Expr::Tuple(items))
                }
            }
            Tok::Ident(_) => {
                let path = self.parse_qualified_name()?;
                Ok(Expr::Var(path))
            }
            other => self.err(format!("expected expression, found {:?}", other)),
        }
    }

    // ----- Contract ----------------------------------------------------------

    fn parse_contract(&mut self) -> R<Contract> {
        self.expect(Tok::Contract)?;
        let name = self.expect_ident()?;
        let mut params = Vec::new();
        if self.eat(&Tok::LParen) {
            if !matches!(self.peek(), Tok::RParen) {
                params.push(self.expect_ident()?);
                while self.eat(&Tok::Comma) {
                    params.push(self.expect_ident()?);
                }
            }
            self.expect(Tok::RParen)?;
        }
        self.expect(Tok::LBrace)?;
        let mut decls = Vec::new();
        while !matches!(self.peek(), Tok::RBrace) {
            decls.push(self.parse_contract_decl()?);
        }
        self.expect(Tok::RBrace)?;
        Ok(Contract { name, params, decls })
    }

    fn parse_contract_decl(&mut self) -> R<ContractDecl> {
        match self.peek() {
            Tok::Data => Ok(ContractDecl::Data(self.parse_data()?)),
            Tok::Constructor => {
                self.bump();
                self.expect(Tok::LParen)?;
                let mut params = Vec::new();
                if !matches!(self.peek(), Tok::RParen) {
                    params.push(self.parse_param()?);
                    while self.eat(&Tok::Comma) {
                        params.push(self.parse_param()?);
                    }
                }
                self.expect(Tok::RParen)?;
                let body = self.parse_body()?;
                Ok(ContractDecl::Constructor(params, body))
            }
            Tok::Forall => {
                let p = self.parse_sig_prefix()?;
                Ok(ContractDecl::Func(self.parse_function(Some(p))?))
            }
            Tok::Function => Ok(ContractDecl::Func(self.parse_function(None)?)),
            Tok::Ident(_) => {
                // Field declaration: name : Type [= init];
                let name = self.expect_ident()?;
                self.expect(Tok::Colon)?;
                let ty = self.parse_type()?;
                let init = if self.eat(&Tok::Eq) { Some(self.parse_expr()?) } else { None };
                self.expect(Tok::Semi)?;
                Ok(ContractDecl::Field(name, ty, init))
            }
            other => self.err(format!("unexpected contract member token {:?}", other)),
        }
    }

    // ----- Assembly (Yul) ----------------------------------------------------

    fn parse_asm_block(&mut self) -> R<AsmBlock> {
        self.expect(Tok::LBrace)?;
        let mut stmts = Vec::new();
        while !matches!(self.peek(), Tok::RBrace) {
            if self.eat(&Tok::Semi) { continue; }
            stmts.push(self.parse_yul_stmt()?);
        }
        self.expect(Tok::RBrace)?;
        Ok(AsmBlock { stmts })
    }

    fn parse_yul_block(&mut self) -> R<Vec<YulStmt>> {
        self.expect(Tok::LBrace)?;
        let mut stmts = Vec::new();
        while !matches!(self.peek(), Tok::RBrace) {
            if self.eat(&Tok::Semi) { continue; }
            stmts.push(self.parse_yul_stmt()?);
        }
        self.expect(Tok::RBrace)?;
        Ok(stmts)
    }

    /// Some SAIL keywords are valid identifiers in Yul (e.g. `return`).
    /// Try to consume one such "yul-ident keyword" and return its textual form.
    fn try_eat_yul_ident_kw(&mut self) -> Option<String> {
        let s = match self.peek() {
            Tok::Return => "return",
            Tok::Default => "default",
            _ => return None,
        };
        self.bump();
        Some(s.to_string())
    }

    fn parse_yul_stmt(&mut self) -> R<YulStmt> {
        // First: handle SAIL keywords that double as Yul identifiers
        // (`return` is the common case used as `return(offset, size)`).
        if let Some(name) = self.try_eat_yul_ident_kw() {
            // It must be a function call as a statement: `return(o, s)`.
            self.expect(Tok::LParen)?;
            let mut args = Vec::new();
            if !matches!(self.peek(), Tok::RParen) {
                args.push(self.parse_yul_expr()?);
                while self.eat(&Tok::Comma) {
                    args.push(self.parse_yul_expr()?);
                }
            }
            self.expect(Tok::RParen)?;
            self.eat(&Tok::Semi);
            return Ok(YulStmt::ExprStmt(YulExpr::Call(name, args)));
        }
        match self.peek() {
            Tok::Let => {
                self.bump();
                let mut names = vec![self.expect_ident()?];
                while self.eat(&Tok::Comma) {
                    names.push(self.expect_ident()?);
                }
                let init = if self.eat(&Tok::Walrus) { Some(self.parse_yul_expr()?) } else { None };
                // optional trailing `;`
                self.eat(&Tok::Semi);
                Ok(YulStmt::Let(names, init))
            }
            Tok::If => {
                self.bump();
                let cond = self.parse_yul_expr()?;
                let body = self.parse_yul_block()?;
                Ok(YulStmt::If(cond, body))
            }
            Tok::Switch => {
                self.bump();
                let scrut = self.parse_yul_expr()?;
                let mut cases = Vec::new();
                let mut default = None;
                loop {
                    match self.peek() {
                        Tok::Case => {
                            self.bump();
                            let v = self.parse_yul_literal()?;
                            let body = self.parse_yul_block()?;
                            cases.push(YulCase { value: v, body });
                        }
                        Tok::Default => {
                            self.bump();
                            default = Some(self.parse_yul_block()?);
                            break;
                        }
                        _ => break,
                    }
                }
                Ok(YulStmt::Switch(scrut, cases, default))
            }
            Tok::For => {
                self.bump();
                let init = self.parse_yul_block()?;
                let cond = self.parse_yul_expr()?;
                let post = self.parse_yul_block()?;
                let body = self.parse_yul_block()?;
                Ok(YulStmt::For(init, cond, post, body))
            }
            Tok::Break => { self.bump(); self.eat(&Tok::Semi); Ok(YulStmt::Break) }
            Tok::Continue => { self.bump(); self.eat(&Tok::Semi); Ok(YulStmt::Continue) }
            Tok::Leave => { self.bump(); self.eat(&Tok::Semi); Ok(YulStmt::Leave) }
            Tok::LBrace => Ok(YulStmt::Block(self.parse_yul_block()?)),
            Tok::Function => {
                self.bump();
                let name = self.expect_ident()?;
                self.expect(Tok::LParen)?;
                let mut params = Vec::new();
                if !matches!(self.peek(), Tok::RParen) {
                    params.push(self.expect_ident()?);
                    while self.eat(&Tok::Comma) {
                        params.push(self.expect_ident()?);
                    }
                }
                self.expect(Tok::RParen)?;
                let mut rets = Vec::new();
                if self.eat(&Tok::Arrow) {
                    rets.push(self.expect_ident()?);
                    while self.eat(&Tok::Comma) {
                        rets.push(self.expect_ident()?);
                    }
                }
                let body = self.parse_yul_block()?;
                Ok(YulStmt::FunctionDef(name, params, rets, body))
            }
            _ => {
                // Either expression statement (function call) or `name [, name]* := expr`.
                // Look ahead for `:=` to decide.
                let save = self.pos;
                if matches!(self.peek(), Tok::Ident(_)) {
                    let mut names = vec![self.expect_ident()?];
                    while self.eat(&Tok::Comma) {
                        if matches!(self.peek(), Tok::Ident(_)) {
                            names.push(self.expect_ident()?);
                        } else { break; }
                    }
                    if self.eat(&Tok::Walrus) {
                        let e = self.parse_yul_expr()?;
                        self.eat(&Tok::Semi);
                        let lhs = names.into_iter().map(YulExprLhs).collect();
                        return Ok(YulStmt::Assign(lhs, e));
                    }
                    // Rewind: it's an expression statement.
                    self.pos = save;
                }
                let e = self.parse_yul_expr()?;
                self.eat(&Tok::Semi);
                Ok(YulStmt::ExprStmt(e))
            }
        }
    }

    fn parse_yul_literal(&mut self) -> R<YulLit> {
        match self.peek().clone() {
            Tok::Int(s) => { self.bump(); Ok(YulLit::Int(s)) }
            Tok::Str(s) => { self.bump(); Ok(YulLit::Str(s)) }
            other => self.err(format!("expected Yul literal, found {:?}", other)),
        }
    }

    fn parse_yul_expr(&mut self) -> R<YulExpr> {
        if let Some(name) = self.try_eat_yul_ident_kw() {
            if self.eat(&Tok::LParen) {
                let mut args = Vec::new();
                if !matches!(self.peek(), Tok::RParen) {
                    args.push(self.parse_yul_expr()?);
                    while self.eat(&Tok::Comma) {
                        args.push(self.parse_yul_expr()?);
                    }
                }
                self.expect(Tok::RParen)?;
                return Ok(YulExpr::Call(name, args));
            }
            return Ok(YulExpr::Var(name));
        }
        match self.peek().clone() {
            Tok::Int(s) => { self.bump(); Ok(YulExpr::Lit(YulLit::Int(s))) }
            Tok::Str(s) => { self.bump(); Ok(YulExpr::Lit(YulLit::Str(s))) }
            Tok::Ident(name) => {
                self.bump();
                if self.eat(&Tok::LParen) {
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Tok::RParen) {
                        args.push(self.parse_yul_expr()?);
                        while self.eat(&Tok::Comma) {
                            args.push(self.parse_yul_expr()?);
                        }
                    }
                    self.expect(Tok::RParen)?;
                    Ok(YulExpr::Call(name, args))
                } else {
                    Ok(YulExpr::Var(name))
                }
            }
            other => self.err(format!("expected Yul expression, found {:?}", other)),
        }
    }
}
