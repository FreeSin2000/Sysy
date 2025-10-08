
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, write};
use std::io::Result;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);
mod ast;
// mod koopa_utils;
// use koopa_utils::*;
use ast::*;

fn main() -> Result<()> {
  // 解析命令行参数
  let mut args = args();
  args.next();
  let mode = args.next().unwrap();
  let input = args.next().unwrap();
  args.next();
  let output = args.next().unwrap();

  // 读取输入文件
  let input = read_to_string(input)?;

  // 调用 lalrpop 生成的 parser 解析输入文件
  let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
  println!("mode:\n{}", mode);
  // 输出解析得到的 AST
  println!("ast:\n{:#?}", ast);
  let mut ast_trans = AstTrans::new();
  ast_trans.enter_scope();
  ast.accept(&mut ast_trans);
  ast_trans.exit_scope();
  let program = &ast_trans.koopa_program; 
  let program_str = koopa_to_string(program);
  println!("koopa:\n{}", program_str);
  
  // let mut koopa_trans = KoopaTrans::new();
  // koopa_trans.pre_analyze(program);
  // let asm_str = koopa_trans.generate_program(program);
  // println!("riscv:\n{}", asm_str);
  // match mode.as_str() {
  //   "-koopa" => write(output, program_str)?,
  //   "-riscv" => write(output, asm_str)?,
  //   _ => todo!("not implement other modes."),
  // }
  Ok(())
}
