use crate::parser::{Expr, Keyword};
use crate::stack_vm::vm::{Instruction, Value, VM};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Compiler {
    global_scope: HashMap<String, SymbolInfo>,
    local_scopes: Vec<HashMap<String, SymbolInfo>>,
    stack_depth: i8,
    proc_stack: Vec<Vec<Instruction>>,
    const_section: Vec<Value>,
    proc_section: Vec<Instruction>,
    main_section: Vec<Instruction>,
}

#[derive(Clone, Debug, Copy)]
enum SymbolKind {
    Local,
    Global,
}

#[derive(Clone, Debug, Copy)]
struct SymbolInfo {
    kind: SymbolKind,
    index: i8,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            global_scope: HashMap::<String, SymbolInfo>::new(),
            local_scopes: vec![],
            stack_depth: 0,
            proc_stack: vec![],
            const_section: vec![],
            proc_section: vec![],
            main_section: vec![],
        }
    }

    pub fn compile(&mut self, exprs: &[Expr]) -> Result<VM, String> {
        self.compile_begin(exprs)?;
        let mut code = self.main_section.clone();
        code.push(Instruction::Halt);
        let addr_offset = code.len();
        code.append(&mut self.proc_section.clone());
        for instr in code.iter_mut() {
            if let Instruction::Call { addr } = instr {
                *addr += addr_offset
            }
        }
        let mut constants = self.const_section.clone();
        for val in constants.iter_mut() {
            if let Value::Procedure { addr } = val {
                *addr += addr_offset
            }
        }
        Ok(VM::new(code, constants))
    }

    fn emit(&mut self, instr: Instruction) {
        self.stack_depth += match instr {
            Instruction::LoadConst { offset: _ } => 1,
            Instruction::LoadGlobal { offset: _ } => 1,
            Instruction::LoadLocal { offset: _ } => 1,
            Instruction::StoreGlobal { offset: _ } => -1,
            Instruction::StoreLocal { offset: _ } => -1,
            Instruction::CallStack => -1,
            Instruction::Slide { n } => -(n as i8),
            Instruction::Drop { n } => -(n as i8),
            Instruction::LessThan | Instruction::LessThanEqual => -1,
            Instruction::GreaterThan | Instruction::GreaterThanEqual => -1,
            Instruction::PushZero | Instruction::PushOne => 1,
            Instruction::Add | Instruction::Sub => -1,
            Instruction::Mul | Instruction::Div => -1,
            _ => 0,
        };
        if let Some(code) = self.proc_stack.last_mut() {
            code.push(instr)
        } else {
            self.main_section.push(instr)
        }
    }

    fn insert_code_at(&mut self, location: usize, instr: Instruction) {
        if let Some(code) = self.proc_stack.last_mut() {
            code.insert(location, instr)
        } else {
            self.main_section.insert(location, instr)
        }
    }

    fn get_section_length(&self) -> usize {
        if let Some(code) = self.proc_stack.last() {
            code.len()
        } else {
            self.main_section.len()
        }
    }

    fn insert_symbol_info(&mut self, s: String) -> SymbolInfo {
        if let Some(local_scope) = self.local_scopes.last_mut() {
            if let Some(info) = local_scope.get(&s) {
                return *info;
            }
            let info = SymbolInfo {
                kind: SymbolKind::Local,
                index: self.stack_depth,
            };
            local_scope.insert(s, info);
            self.stack_depth += 1;
            return info;
        }
        if let Some(info) = self.global_scope.get(&s) {
            return *info;
        }
        let info = SymbolInfo {
            kind: SymbolKind::Global,
            index: self.global_scope.len() as i8,
        };
        self.global_scope.insert(s, info);
        info
    }

    fn get_symbol_info(&self, symbol: &String) -> Option<SymbolInfo> {
        for env in self.local_scopes.iter().rev() {
            if let Some(info) = env.get(symbol) {
                return Some(*info);
            }
        }
        if let Some(info) = self.global_scope.get(symbol) {
            return Some(*info);
        }
        None
    }

    fn get_or_insert_const(&mut self, val: Value) -> u8 {
        if let Some(idx) = self.const_section.iter().position(|x| x == &val) {
            idx as u8
        } else {
            let idx = self.const_section.len();
            self.const_section.push(val);
            idx as u8
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::List(v) => self.compile_list(v),
            Expr::Bool(b) => {
                let offset = self.get_or_insert_const(Value::Bool(*b));
                self.emit(Instruction::LoadConst { offset });
                Ok(())
            }
            Expr::Integer(i) => {
                let offset = self.get_or_insert_const(Value::Int(*i));
                self.emit(Instruction::LoadConst { offset });
                Ok(())
            }
            Expr::Float(f) => {
                let offset = self.get_or_insert_const(Value::Float(*f));
                self.emit(Instruction::LoadConst { offset });
                Ok(())
            }
            Expr::Symbol(s) => {
                let Some(info) = self.get_symbol_info(s) else {
                    return Err(format!("Not found in scope: {s}"));
                };
                let instr = match info.kind {
                    SymbolKind::Global => Instruction::LoadGlobal {
                        offset: info.index as u8,
                    },
                    SymbolKind::Local => Instruction::LoadLocal { offset: info.index },
                };
                self.emit(instr);
                Ok(())
            }
            _ => Err(format!("Cannot compile expression: {expr}")),
        }
    }

    fn compile_list(&mut self, exprs: &[Expr]) -> Result<(), String> {
        let Some((first, rest)) = exprs.split_first() else {
            return Err("Cannot compile the empty list".to_string());
        };
        match first {
            Expr::Keyword(Keyword::If) => self.compile_if(rest),
            Expr::Keyword(Keyword::Begin) => self.compile_begin(rest),
            Expr::Keyword(Keyword::Define) => self.compile_define(rest),
            Expr::Keyword(Keyword::Let) => self.compile_let(rest),
            Expr::Keyword(Keyword::Lambda) => {
                self.proc_stack.push(vec![]);
                self.compile_lambda(rest)?;
                let Some(mut code) = self.proc_stack.pop() else {
                    return Err("unreachable".to_string());
                };
                let addr = self.proc_section.len();
                let offset = self.get_or_insert_const(Value::Procedure { addr });
                self.proc_section.append(&mut code);
                self.emit(Instruction::LoadConst { offset });
                Ok(())
            }
            Expr::Symbol(s) => {
                let s = s.as_str();
                match s {
                    "<" | ">" | "<=" | ">=" => self.compile_cmp(s, rest),
                    "+" => self.compile_add(rest),
                    "-" => self.compile_sub(rest),
                    "*" => self.compile_mul(rest),
                    "/" => self.compile_div(rest),
                    "abs" => self.compile_abs(rest),
                    "cons" => self.compile_cons(rest),
                    "car" => self.compile_car(rest),
                    "cdr" => self.compile_cdr(rest),
                    "null?" => self.compile_isnull(rest),
                    _ => {
                        for expr in rest.iter().rev() {
                            self.compile_expr(expr)?;
                        }
                        self.compile_expr(first)?;
                        self.emit(Instruction::CallStack);
                        self.emit(Instruction::Slide { n: rest.len() });
                        Ok(())
                    }
                }
            }
            Expr::List(v) => {
                for expr in rest.iter().rev() {
                    self.compile_expr(expr)?;
                }
                self.compile_list(v)?;
                self.emit(Instruction::CallStack);
                self.emit(Instruction::Slide { n: rest.len() });
                Ok(())
            }
            Expr::Keyword(Keyword::Quote) => {
                let [arg] = rest else {
                    return Err("Quote needs exactly one argument".to_string());
                };
                let value = Value::from(arg.clone());
                let offset = self.get_or_insert_const(value);
                self.emit(Instruction::LoadConst { offset });
                Ok(())
            }
            _ => todo!("list starting with: {first:?}"),
        }
    }

    fn compile_if(&mut self, args: &[Expr]) -> Result<(), String> {
        let [cond, branch_true, branch_false] = args else {
            return Err("`if` takes exactly 3 arguments".to_string());
        };
        self.compile_expr(cond)?;
        // JumpIfTrue will pop the condition at runtime
        self.stack_depth -= 1;
        let depth_before_branch = self.stack_depth;
        let len = self.get_section_length();
        self.compile_expr(branch_false)?;
        let len_branch_false = self.get_section_length();
        // reset depth for the other branch (they're alternatives)
        self.stack_depth = depth_before_branch;
        self.compile_expr(branch_true)?;
        let len_branch_true = self.get_section_length();
        self.insert_code_at(
            len,
            Instruction::JumpIfTrue {
                offset: (len_branch_false - len) as i16 + 1,
            },
        );
        self.insert_code_at(
            len_branch_false + 1,
            Instruction::Jump {
                offset: (len_branch_true - len_branch_false) as i16,
            },
        );
        Ok(())
    }

    fn compile_begin(&mut self, args: &[Expr]) -> Result<(), String> {
        for expr in args.iter() {
            self.compile_expr(expr)?;
        }
        Ok(())
    }

    fn compile_define(&mut self, args: &[Expr]) -> Result<(), String> {
        let mut args = args.to_vec();
        if let Some((Expr::List(l), body)) = args.split_first() {
            // Rewrite: (define (f as...) exprs...)
            // As: (define f (lambda (as...) exprs...))
            let Some((Expr::Symbol(s), formals)) = l.split_first() else {
                return Err("`define` with a list needs a non-empty list of symbols".to_string());
            };
            let mut lambda = vec![Expr::Keyword(Keyword::Lambda), Expr::List(formals.to_vec())];
            lambda.append(&mut body.to_vec());
            args = vec![Expr::Symbol(*s), Expr::List(lambda)];
        }
        let [Expr::Symbol(s), expr] = args.as_slice() else {
            // Assume: (define a expr)
            return Err("`define` takes 2 arguments".to_string());
        };
        let info = self.insert_symbol_info(s.to_string());
        let store_instr = match info.kind {
            SymbolKind::Global => Instruction::StoreGlobal {
                offset: info.index as u8,
            },
            SymbolKind::Local => Instruction::StoreLocal { offset: info.index },
        };
        self.compile_expr(expr)?;
        self.emit(store_instr);
        Ok(())
    }

    fn compile_let(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((Expr::List(bindings), body)) = args.split_first() else {
            return Err("`let`: needs at least 1 argument (a list of bindings)".to_string());
        };
        // save stack depth at entry
        let base_depth = self.stack_depth;
        // create local scope
        let mut local_scope = HashMap::<String, SymbolInfo>::new();
        // compile bindings — each compile_expr pushes a value and increments stack_depth
        for binding in bindings.iter() {
            let Expr::List(v) = binding else {
                return Err("`let`: each binding must be a list of two elements".to_string());
            };
            let [Expr::Symbol(s), value] = v.as_slice() else {
                return Err("`let`: each binding must be a list of two elements".to_string());
            };
            // the binding value will land at current stack_depth
            local_scope.insert(
                s.to_string(),
                SymbolInfo {
                    kind: SymbolKind::Local,
                    index: self.stack_depth,
                },
            );
            // compile binding expression (increments stack_depth)
            self.compile_expr(value)?;
        }
        // enter scope, compile body, exit scope
        self.local_scopes.push(local_scope);
        self.compile_begin(body)?;
        self.local_scopes.pop();
        // slide: keep the result on top, drop all slots allocated since entry
        let slots_to_drop = (self.stack_depth - base_depth - 1) as usize;
        if slots_to_drop > 0 {
            self.emit(Instruction::Slide { n: slots_to_drop });
        }
        Ok(())
    }

    fn compile_lambda(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((Expr::List(arguments), body)) = args.split_first() else {
            return Err("`lambda`: needs at least 1 argument (a list of arguments)".to_string());
        };
        // save and reset stack depth for new frame
        let saved_depth = self.stack_depth;
        self.stack_depth = 0;
        // create local scope
        let mut local_scope = HashMap::<String, SymbolInfo>::new();
        for (idx, argument) in arguments.iter().enumerate() {
            let Expr::Symbol(s) = argument else {
                return Err("`lambda`: the list of arguments must contain symbols".to_string());
            };
            local_scope.insert(
                s.to_string(),
                SymbolInfo {
                    kind: SymbolKind::Local,
                    // NOTE: index is set based on the calling conventions
                    index: -(idx as i8 + 3),
                },
            );
        }
        self.local_scopes.push(local_scope);
        self.compile_begin(body)?;
        self.emit(Instruction::Ret);
        self.local_scopes.pop();
        // restore stack depth
        self.stack_depth = saved_depth;
        Ok(())
    }

    fn compile_cmp(&mut self, cmp: &str, args: &[Expr]) -> Result<(), String> {
        let [left, right] = args else {
            return Err(format!("`{cmp}` takes exactly 2 arguments"));
        };
        self.compile_expr(left)?;
        self.compile_expr(right)?;
        match cmp {
            "<" => self.emit(Instruction::LessThan),
            "<=" => self.emit(Instruction::LessThanEqual),
            ">" => self.emit(Instruction::GreaterThan),
            ">=" => self.emit(Instruction::GreaterThanEqual),
            _ => todo!("comparison operator: {cmp}"),
        }
        Ok(())
    }

    fn compile_add(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((first, mut rest)) = args.split_first() else {
            self.emit(Instruction::PushZero);
            return Ok(());
        };
        self.compile_expr(first)?;
        while let Some((first, next_rest)) = rest.split_first() {
            self.compile_expr(first)?;
            self.emit(Instruction::Add);
            rest = next_rest;
        }
        Ok(())
    }

    fn compile_sub(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((first, mut rest)) = args.split_first() else {
            return Err("`-` takes at least one argument".to_string());
        };
        if rest.is_empty() {
            self.emit(Instruction::PushZero);
            self.compile_expr(first)?;
            self.emit(Instruction::Sub);
            return Ok(());
        }
        self.compile_expr(first)?;
        while let Some((first, next_rest)) = rest.split_first() {
            self.compile_expr(first)?;
            self.emit(Instruction::Sub);
            rest = next_rest;
        }
        Ok(())
    }

    fn compile_mul(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((first, mut rest)) = args.split_first() else {
            self.emit(Instruction::PushOne);
            return Ok(());
        };
        self.compile_expr(first)?;
        while let Some((first, next_rest)) = rest.split_first() {
            self.compile_expr(first)?;
            self.emit(Instruction::Mul);
            rest = next_rest;
        }
        Ok(())
    }

    fn compile_div(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((first, mut rest)) = args.split_first() else {
            return Err("`/` takes at least one argument".to_string());
        };
        if rest.is_empty() {
            self.emit(Instruction::PushOne);
            self.compile_expr(first)?;
            self.emit(Instruction::Div);
            return Ok(());
        }
        self.compile_expr(first)?;
        while let Some((first, next_rest)) = rest.split_first() {
            self.compile_expr(first)?;
            self.emit(Instruction::Div);
            rest = next_rest;
        }
        Ok(())
    }

    fn compile_abs(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((first, rest)) = args.split_first() else {
            return Err("`abs` expects one argument".to_string());
        };
        if !rest.is_empty() {
            return Err("`abs` expects one argument".to_string());
        }
        self.compile_expr(first)?;
        self.emit(Instruction::Abs);
        Ok(())
    }

    fn compile_cons(&mut self, args: &[Expr]) -> Result<(), String> {
        let [car, cdr] = args else {
            return Err("`cons` takes exactly 2 arguments".to_string());
        };
        self.compile_expr(car)?;
        self.compile_expr(cdr)?;
        self.emit(Instruction::Cons);
        Ok(())
    }

    fn compile_isnull(&mut self, args: &[Expr]) -> Result<(), String> {
        let [arg] = args else {
            return Err("`null?` takes exactly 1 argument".to_string());
        };
        self.compile_expr(arg)?;
        self.emit(Instruction::IsNull);
        Ok(())
    }

    fn compile_car(&mut self, args: &[Expr]) -> Result<(), String> {
        let [pair] = args else {
            return Err("`car` takes exactly 1 arguments".to_string());
        };
        self.compile_expr(pair)?;
        self.emit(Instruction::Car);
        Ok(())
    }

    fn compile_cdr(&mut self, args: &[Expr]) -> Result<(), String> {
        let [pair] = args else {
            return Err("`cdr` takes exactly 1 arguments".to_string());
        };
        self.compile_expr(pair)?;
        self.emit(Instruction::Cdr);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::parser::parse;

    #[test]
    fn test_parse_compile_run() {
        let cases = vec![
            // basic expressions and operations
            ("#t", "#t"),
            ("#f", "#f"),
            ("42", "42"),
            ("42.", "42.0"),
            ("42.0", "42.0"),
            ("(< 2 3)", "#t"),
            ("(< 3 3)", "#f"),
            ("(< 4 3)", "#f"),
            ("(<= 2 3)", "#t"),
            ("(<= 3 3)", "#t"),
            ("(<= 4 3)", "#f"),
            ("(> 2 3)", "#f"),
            ("(> 3 3)", "#f"),
            ("(> 4 3)", "#t"),
            ("(>= 2 3)", "#f"),
            ("(>= 3 3)", "#t"),
            ("(>= 4 3)", "#t"),
            ("(+ 3)", "3"),
            ("(+ 3 4 5 6)", "18"),
            ("(- 7)", "-7"),
            ("(- 7 4)", "3"),
            ("(- 3 4 5 6)", "-12"),
            ("(* 3)", "3"),
            ("(* 3 4 5)", "60"),
            ("(/ 4.0)", "0.25"),
            ("(/ 4.0 2)", "2.0"),
            ("(/ 1.0 2.0 2 2)", "0.125"),
            ("(abs 42)", "42"),
            ("(abs -42)", "42"),
            ("(cons 7 13)", "(7 . 13)"),
            ("(cons 7 '())", "(7)"),
            ("(car (cons 7 13))", "7"),
            ("(cdr (cons 7 13))", "13"),
            ("'#t", "#t"),
            ("'#f", "#f"),
            ("'42", "42"),
            ("'()", "()"),
            ("'(1 2 3)", "(1 2 3)"),
            ("'(/ 1.0 2 3)", "(/ 1.0 2 3)"),
            // pairs and lists
            ("(car '(1 2 3))", "1"),
            ("(cdr '(1 2 3))", "(2 3)"),
            ("'(4 . 5)", "(4 . 5)"),
            ("(car '(4 . 5))", "4"),
            ("(cdr '(4 . 5))", "5"),
            ("(null? '())", "#t"),
            ("(null? '(1))", "#f"),
            ("(null? 4.2)", "#f"),
            ("(null? 42)", "#f"),
            // if then else
            ("(if #t 1 0)", "1"),
            ("(if #f 1 0)", "0"),
            ("(if (< 2 3) 1 0)", "1"),
            ("(if (< 3 2) 1 0)", "0"),
            ("(if (< 2 3) (+ 1 4) (- 3 7))", "5"),
            ("(if (< 3 2) (+ 1 4) (- 3 7))", "-4"),
            // define, begin, and multiple expressions
            ("(begin (+ 3 4) (- 7 (if (< 4 5) 1 0)))", "6"),
            ("(define x 3)", ""),
            ("(begin (define x 3) x)", "3"),
            ("(begin (define x 3) (define x 4) x)", "4"),
            ("(begin (define y #t) (define x 6) y)", "#t"),
            ("(define x 3) (define x 4) x", "4"),
            ("(define y #f) (define x 6) y", "#f"),
            ("(define y #f) (define x 6) (define y 42) y", "42"),
            ("(define a (cons 3 5)) (define b (cons 7 a)) (car b)", "7"),
            (
                "(define a (cons 3 5)) (define b (cons 7 a)) (car (cdr b))",
                "3",
            ),
            (
                "(define a (cons 3 5)) (define b (cons 7 a)) (cdr (cdr b))",
                "5",
            ),
            // let
            ("(let ((a 3)) (+ a 1))", "4"),
            ("(let ((a 3) (b 4)) (+ a b))", "7"),
            // interaction between let and define
            (
                "(let ((a 3) (b 4)) (define c 5) (define d 6) (+ a b c d))",
                "18",
            ),
            ("(let ((a 1)) (define a 99) a)", "99"),
            ("(let ((a 1)) (let ((b 2)) b))", "2"),
            ("(let ((a 10)) a) (let ((b 20)) b)", "20"),
            ("(define a 3) (let ((a 15)) (+ a 4) (+ a 2))", "17"),
            ("(define a 3) (+ a (let ((a 42)) (+ a 1)))", "46"),
            // nested let accessing both scopes
            ("(let ((a 1)) (let ((b 2)) (+ a b)))", "3"),
            // let inside if branch
            ("(if #t (let ((x 10)) (+ x 1)) 0)", "11"),
            ("(if #f 0 (let ((x 20)) (+ x 1)))", "21"),
            // lambda basics
            ("(lambda (x) (+ x 1))", "$Procedure@2"),
            ("((lambda (x) (+ x 1)) 3)", "4"),
            (
                r#"
                (define a 3)
                (define plus-a (lambda (x) (+ a x)))
                (plus-a 42)
                "#,
                "45",
            ),
            (
                r#"
                (define a 3)
                (define plus-a (lambda (x) (+ a x)))
                (define a 16)
                (plus-a 42)
                "#,
                "58",
            ),
            (
                r#"
                (define f (lambda (x) (define g (lambda (y) (+ 3 y))) (g x)))
                (f 4)
                "#,
                "7",
            ),
            // define as syntactic sugar for lambda
            (
                r#"
                (define (f x y) (+ x y) (* x y))
                (f 4 7)
                "#,
                "28",
            ),
            // redefining procedures
            (
                r#"
                (define (f x) (* 2 x))
                (define (f x) (* 3 x))
                (f 7)
                "#,
                "21",
            ),
            // let inside a lambda body
            (
                r#"
                (define f (lambda (x)
                    (let ((y (+ x 1)))
                        (* x y))))
                (f 5)
                "#,
                "30",
            ),
            // define then let inside lambda
            (
                r#"
                (define f (lambda (x)
                    (define y 10)
                    (let ((z 20))
                        (+ x y z))))
                (f 5)
                "#,
                "35",
            ),
            // recursion
            (
                r#"
                (define count (lambda (m n)
                    (if (>= m n)
                        m
                        (count (+ m 1) n))))
                (count 0 10)
                "#,
                "10",
            ),
            (
                r#"
                (define fib (lambda (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1)) (fib (- n 2))))))
                (fib 7)
                "#,
                "13",
            ),
            // newton method for square root
            (
                r#"
                (define (square x) (* x x))
                (define (average x y) (/ (+ x y) 2))
                (define (good-enough? guess x)
                    (< (abs (- (square guess) x)) 0.001))
                (define (improve guess x)
                    (average guess (/ x guess)))
                (define (sqrt-iter guess x)
                    (if (good-enough? guess x)
                        guess
                        (sqrt-iter (improve guess x) x)))
                (define (sqrt x)
                    (sqrt-iter 1.0 x))
                (sqrt 2)
                "#,
                "1.4142156862745097",
            ),
            // returning procedures
            (
                r#"
                (define make-inc (lambda ()
                    (lambda (x)
                            (+ 1 x))))
                (define inc (make-inc))
                (inc 4)
                "#,
                "5",
            ),
            // returning closures
            // (
            //     r#"
            //     (define make-inc-by (lambda (a)
            //         (lambda (x)
            //                 (+ a x))))
            //     (define a 13)
            //     (define inc-by-a (make-inc-by a))
            //     (define a 42)
            //     (inc-by-a 6)"#,
            //     "19",
            // ),
        ];

        for (code, expected_res) in cases {
            let exprs = parse(code).unwrap();
            let mut vm = Compiler::new().compile(&exprs).unwrap();
            vm.debug();
            assert_eq!(
                vm.clone_stack_top()
                    .map(|x| x.to_string())
                    .unwrap_or("".into()),
                expected_res,
                "we are testing `{code}`"
            )
        }
    }

    #[test]
    fn test_compilation_errors() {
        let cases = vec![
            (
                "(let ((a 3) (b 4)) (+ a b)) a",
                Err("Not found in scope: a".into()),
            ),
            ("a", Err("Not found in scope: a".into())),
            ("(define a 3) b", Err("Not found in scope: b".into())),
            (
                "(define f (lambda (x) (define g (lambda (y) (+ 3 y))) (g x))) (g 4)",
                Err("Not found in scope: g".into()),
            ),
            (
                "(let ((a 3) (b (+ a 1))) b)",
                Err("Not found in scope: a".into()),
            ),
        ];

        for (code, expected_res) in cases {
            let exprs = parse(code).unwrap();
            let mut compiler = Compiler::new();
            assert_eq!(compiler.compile(&exprs), expected_res);
        }
    }

    #[test]
    #[should_panic]
    fn test_car_errors() {
        let exprs = parse("(car '())").unwrap();
        let mut vm = Compiler::new().compile(&exprs).unwrap();
        vm.debug();
    }

    #[test]
    #[should_panic]
    fn test_cdr_errors() {
        let exprs = parse("(cdr '())").unwrap();
        let mut vm = Compiler::new().compile(&exprs).unwrap();
        vm.debug();
    }
}
