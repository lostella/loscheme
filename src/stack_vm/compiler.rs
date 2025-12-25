use crate::parser::{Expr, Keyword};
use crate::stack_vm::vm::{Instruction, Value, VM};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Compiler {
    global_scope: HashMap<String, SymbolInfo>,
    local_scopes: Vec<HashMap<String, SymbolInfo>>,
    proc_section: Vec<Instruction>,
    main_section: Vec<Instruction>,
    emit_to_main: bool,
}

#[derive(Clone, Debug)]
enum SymbolKind {
    Local,
    Global,
}

#[derive(Clone, Debug)]
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
            proc_section: vec![],
            main_section: vec![],
            emit_to_main: true,
        }
    }

    pub fn compile(&mut self, exprs: &[Expr]) -> Result<VM, String> {
        self.compile_begin(exprs)?;
        let mut code = self.proc_section.clone();
        code.append(&mut self.main_section.clone());
        Ok(VM::new(code, self.proc_section.len()))
    }

    fn emit(&mut self, instr: Instruction) {
        if self.emit_to_main {
            self.main_section.push(instr)
        } else {
            self.proc_section.push(instr)
        }
    }

    fn insert_code_at(&mut self, location: usize, instr: Instruction) {
        if self.emit_to_main {
            self.main_section.insert(location, instr)
        } else {
            self.proc_section.insert(location, instr)
        }
    }

    fn get_section_length(&self) -> usize {
        if self.emit_to_main {
            self.main_section.len()
        } else {
            self.proc_section.len()
        }
    }

    fn get_symbol_info(&self, symbol: &String) -> Option<SymbolInfo> {
        for env in self.local_scopes.iter().rev() {
            if let Some(info) = env.get(symbol) {
                return Some(info.clone());
            }
        }
        if let Some(info) = self.global_scope.get(symbol) {
            return Some(info.clone());
        }
        None
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::List(v) => self.compile_list(v),
            Expr::Bool(b) => {
                self.emit(Instruction::Push {
                    value: Value::Bool(*b),
                });
                Ok(())
            }
            Expr::Integer(i) => {
                self.emit(Instruction::Push {
                    value: Value::Int(*i),
                });
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
                let was_emitting_to_main = self.emit_to_main;
                self.emit_to_main = false;
                let addr = self.get_section_length();
                let res = self.compile_lambda(rest);
                self.emit_to_main = was_emitting_to_main;
                self.emit(Instruction::Push {
                    value: Value::Procedure { addr },
                });
                res
            }
            Expr::Symbol(s) => {
                let s = s.as_str();
                match s {
                    "<" | ">" | "<=" | ">=" => self.compile_cmp(s, rest),
                    "+" => self.compile_add(rest),
                    "-" => self.compile_sub(rest),
                    _ => {
                        for expr in rest.iter().rev() {
                            self.compile_expr(expr)?
                        }
                        self.compile_expr(first)?;
                        self.emit(Instruction::CallStack);
                        Ok(())
                    }
                }
            }
            Expr::List(v) => {
                for expr in rest.iter().rev() {
                    self.compile_expr(expr)?
                }
                self.compile_list(v)?;
                self.emit(Instruction::CallStack);
                Ok(())
            }
            _ => todo!("list starting with: {first:?}"),
        }
    }

    fn compile_if(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.len() != 3 {
            return Err("`if` takes exactly 3 arguments".to_string());
        }
        self.compile_expr(&args[0])?;
        let len = self.get_section_length();
        self.compile_expr(&args[2])?;
        let len_branch_false = self.get_section_length();
        self.compile_expr(&args[1])?;
        let len_branch_true = self.get_section_length();
        self.insert_code_at(
            len,
            Instruction::JumpIfTrue {
                offset: (len_branch_false - len) as i16 + 1,
            },
        );
        self.insert_code_at(
            len_branch_false + 1,
            Instruction::JumpOffset {
                offset: len_branch_true as i16,
            },
        );
        Ok(())
    }

    fn compile_begin(&mut self, args: &[Expr]) -> Result<(), String> {
        for expr in args {
            self.compile_expr(expr)?
        }
        Ok(())
    }

    fn compile_define(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((Expr::Symbol(s), rest)) = args.split_first() else {
            return Err("`define` takes at least 1 argument".to_string());
        };
        for expr in rest {
            self.compile_expr(expr)?;
        }
        let (kind, env) = match self.local_scopes.last_mut() {
            Some(env) => (SymbolKind::Local, env),
            _ => (SymbolKind::Global, &mut self.global_scope),
        };
        let key = s.to_string();
        if !env.contains_key(&key) {
            env.insert(
                key.clone(),
                SymbolInfo {
                    kind,
                    index: env.len() as i8,
                },
            );
        }
        let Some(info) = env.get(&key) else {
            return Err("Cannot find symbol in environment, this should not happen".to_string());
        };
        let last_instr = match info.kind {
            SymbolKind::Global => Instruction::StoreGlobal {
                offset: info.index as u8,
            },
            SymbolKind::Local => Instruction::StoreLocal { offset: info.index },
        };
        self.emit(last_instr);
        Ok(())
    }

    fn compile_let(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((Expr::List(bindings), body)) = args.split_first() else {
            return Err("`let`: needs at least 1 argument (a list of bindings)".to_string());
        };
        // create local scope
        let mut local_scope = HashMap::<String, SymbolInfo>::new();
        // compile bindings
        for (idx, binding) in bindings.iter().enumerate() {
            let Expr::List(v) = binding else {
                return Err("`let`: each binding must be a list of two elements".to_string());
            };
            if v.len() != 2 {
                return Err("`let`: each binding must be a list of two elements".to_string());
            };
            let Expr::Symbol(s) = v[0] else {
                return Err("`let`: first element of each binding must be a symbol".to_string());
            };
            // add binding variable to local scope
            local_scope.insert(
                s.to_string(),
                SymbolInfo {
                    kind: SymbolKind::Local,
                    index: idx as i8,
                },
            );
            // compile binding expression
            self.compile_expr(&v[1])?;
        }
        // enter scope, compile body, exit scope
        self.local_scopes.push(local_scope);
        self.compile_begin(body)?;
        self.local_scopes.pop();
        Ok(())
    }

    fn compile_lambda(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((Expr::List(arguments), body)) = args.split_first() else {
            return Err("`lambda`: needs at least 1 argument (a list of arguments)".to_string());
        };
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
        Ok(())
    }

    fn compile_cmp(&mut self, cmp: &str, args: &[Expr]) -> Result<(), String> {
        if args.len() != 2 {
            return Err(format!("`{cmp}` takes exactly 2 arguments"));
        }
        self.compile_expr(&args[1])?;
        self.compile_expr(&args[0])?;
        match cmp {
            "<" => self.emit(Instruction::LessThan),
            _ => todo!("comparison operator: {cmp}"),
        }
        Ok(())
    }

    fn compile_add(&mut self, args: &[Expr]) -> Result<(), String> {
        let Some((first, mut rest)) = args.split_first() else {
            self.emit(Instruction::Push {
                value: Value::Int(0),
            });
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
        if args.len() != 2 {
            return Err("`-` takes exactly 2 arguments".to_string());
        }
        self.compile_expr(&args[1])?;
        self.compile_expr(&args[0])?;
        self.emit(Instruction::Sub);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Compiler, Value::*};
    use crate::parser::parse;

    #[test]
    fn test_parse_compile_run() {
        let cases = vec![
            ("#t", Some(Bool(true))),
            ("#f", Some(Bool(false))),
            ("42", Some(Int(42))),
            ("(+ 3 4 5 6)", Some(Int(18))),
            ("(if #t 1 0)", Some(Int(1))),
            ("(if #f 1 0)", Some(Int(0))),
            ("(if (< 2 3) 1 0)", Some(Int(1))),
            ("(if (< 3 2) 1 0)", Some(Int(0))),
            ("(if (< 2 3) (+ 1 4) (- 3 7))", Some(Int(5))),
            ("(if (< 3 2) (+ 1 4) (- 3 7))", Some(Int(-4))),
            ("(begin (+ 3 4) (- 7 (if (< 4 5) 1 0)))", Some(Int(6))),
            ("(define x 3)", None),
            ("(begin (define x 3) x)", Some(Int(3))),
            ("(begin (define x 3) (define x 4) x)", Some(Int(4))),
            ("(begin (define y #t) (define x 6) y)", Some(Bool(true))),
            ("(define x 3) (define x 4) x", Some(Int(4))),
            ("(define y #f) (define x 6) y", Some(Bool(false))),
            ("(define y #f) (define x 6) (define y 42) y", Some(Int(42))),
            ("(let ((a 3)) (+ a 1))", Some(Int(4))),
            ("(let ((a 3) (b 4)) (+ a b))", Some(Int(7))),
            (
                "(let ((a 3) (b 4)) (define c 5) (define d 6) (+ a b c d))",
                Some(Int(18)),
            ),
            ("(define a 3) (let ((a 15)) (+ a 4) (+ a 2))", Some(Int(17))),
            ("(define a 3) (+ a (let ((a 42)) (+ a 1)))", Some(Int(46))),
            ("(lambda (x) (+ x 1))", Some(Procedure { addr: 0 })),
            ("((lambda (x) (+ x 1)) 3)", Some(Int(4))),
            (
                "(define a 3) (define plus-a (lambda (x) (+ a x))) (plus-a 42)",
                Some(Int(45)),
            ),
            (
                "(define a 3) (define plus-a (lambda (x) (+ a x))) (define a 16) (plus-a 42)",
                Some(Int(58)),
            ),
            // (
            //     "(define make-adder (lambda (a) (lambda (x) (+ a x)))) (define a 5) (define plus-a (make-adder a)) (define a 42) (plus-a 6)",
            //     Some(Int(48)),
            // ),
        ];

        for (code, expected_res) in cases {
            let exprs = parse(code).unwrap();
            let mut vm = Compiler::new().compile(&exprs).unwrap();
            vm.debug().unwrap();
            assert_eq!(
                vm.clone_stack_top(),
                expected_res,
                "we are testing `{code}`"
            )
        }
    }

    #[test]
    fn test_compilation_errors() {
        let cases = vec![(
            "(let ((a 3) (b 4)) (+ a b)) a",
            Err("Not found in scope: a".into()),
        )];

        for (code, expected_res) in cases {
            let exprs = parse(code).unwrap();
            let mut compiler = Compiler::new();
            assert_eq!(compiler.compile(&exprs), expected_res);
        }
    }
}
