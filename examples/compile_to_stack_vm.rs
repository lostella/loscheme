use loscheme::parser::{parse, Expr, Keyword};
use loscheme::stack_vm::{
    Instruction::{self},
    Value::{self},
    VM,
};

struct Compiler {}

impl Compiler {
    fn new() -> Self {
        Compiler {}
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Vec<Instruction>, String> {
        match expr {
            Expr::List(v) => self.compile_list(&v),
            Expr::Bool(b) => Ok(vec![Instruction::Push {
                value: Value::Bool(*b),
            }]),
            Expr::Integer(i) => Ok(vec![Instruction::Push {
                value: Value::Int(*i),
            }]),
            _ => todo!("{expr}"),
        }
    }

    fn compile_list(&mut self, exprs: &[Expr]) -> Result<Vec<Instruction>, String> {
        let Some((first, rest)) = exprs.split_first() else {
            return Err("Cannot compile the empty list".to_string());
        };
        match first {
            Expr::Keyword(Keyword::If) => self.compile_if(rest),
            Expr::Keyword(Keyword::Lambda) => self.compile_lambda(rest),
            Expr::Symbol(s) => {
                let s = s.as_str();
                match s {
                    "<" | ">" | "<=" | ">=" => self.compile_cmp(s, rest),
                    "+" => self.compile_add(rest),
                    "-" => self.compile_sub(rest),
                    _ => todo!("list starting with: {first}"),
                }
            }
            _ => todo!("list starting with: {first}"),
        }
    }

    fn compile_if(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 3 {
            return Err("`if` takes exactly 3 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut branch_true = self.compile_expr(&args[1])?;
        let mut branch_false = self.compile_expr(&args[2])?;
        instr.push(Instruction::JumpIfTrue {
            offset: branch_false.len() as i16 + 2,
        });
        instr.append(&mut branch_false);
        instr.push(Instruction::Jump {
            offset: branch_true.len() as i16 + 1,
        });
        instr.append(&mut branch_true);
        Ok(instr)
    }

    fn compile_cmp(&mut self, cmp: &str, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 2 {
            return Err("`{cmp}` takes exactly 2 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut instr_op2 = self.compile_expr(&args[1])?;
        instr.append(&mut instr_op2);
        match cmp {
            "<" => instr.push(Instruction::LessThan),
            _ => todo!("comparison operator: {cmp}"),
        }
        Ok(instr)
    }

    fn compile_add(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 2 {
            return Err("`+` takes exactly 2 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut instr_op2 = self.compile_expr(&args[1])?;
        instr.append(&mut instr_op2);
        instr.push(Instruction::Add);
        Ok(instr)
    }

    fn compile_sub(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 2 {
            return Err("`-` takes exactly 2 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut instr_op2 = self.compile_expr(&args[1])?;
        instr.append(&mut instr_op2);
        instr.push(Instruction::Sub);
        Ok(instr)
    }

    fn compile_lambda(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() < 1 {
            return Err("`lambda` takes at least 1 argument".to_string());
        }
        todo!("lambda")
    }
}

fn compile(exprs: &[Expr]) -> Result<Vec<Instruction>, String> {
    if exprs.len() != 1 {
        return Err("Only one expression can be compiled (for now)".to_string());
    }
    let mut comp = Compiler::new();
    comp.compile_expr(&exprs[0])
}

fn run_example(name: &str, code: &str) {
    println!("-------------------------- {name}");
    println!(" code: {code:?}");
    let exprs = parse(code).unwrap();
    println!("exprs: {exprs:?}");
    let instr = compile(&exprs).unwrap();
    println!("instr: {instr:?}");
    let mut vm = VM::new(instr);
    let res = vm.run();
    println!("  res: {res:?}");
}

fn main() {
    run_example("if", "(if #t 1 0)");
    run_example("if_cmp", "(if (< 3 2) 1 0)");
    run_example("if_cmp_add_sub", "(if (< 3 2) (+ 1 4) (- 3 7))");
    run_example("lambda", "(lambda (x) (+ x 1))");
    run_example(
        "fib",
        "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)",
    );
}
