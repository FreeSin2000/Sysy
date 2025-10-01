use koopa::ir::*;
use koopa::ir::layout::*;
use koopa::ir::builder_traits::*;

use koopa::back::KoopaGenerator;
use koopa::back::generator::NameManager;

use std::fmt::Write;

pub fn program_to_string(program: & Program) -> String {
    let mut gen = KoopaGenerator::new(Vec::new());
    gen.generate_on(program).unwrap();
    let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
    return text_form_ir;
}


// 根据内存形式 Koopa IR 生成汇编
pub trait GenerateAsm {
  fn generate(&self, nm: &mut NameManager, asm_str: &mut String);
}

impl GenerateAsm for Program {
  fn generate(&self, nm: &mut NameManager, asm_str: &mut String) {
    asm_str.push_str("  .text\n");
    for &func in self.func_layout() {
        let func_name = self.func(func).name();
        asm_str.push_str(&format!("  .global {}\n", &func_name[1..]));
    }
    for &func in self.func_layout() {
        nm.enter_func_scope();
        self.func(func).generate(nm, asm_str);
        nm.exit_func_scope();
    }
  }
}

impl GenerateAsm for FunctionData {
  fn generate(&self, nm: &mut NameManager, asm_str: &mut String) {
    let func_label = format!("{}:\n", &self.name()[1..]);
    asm_str.push_str(&func_label);

    for (bb, bb_node) in self.layout().bbs() {
        let bb_data = self.dfg().bb(*bb);
        let bb_name = &bb_data.name().clone().unwrap();
        asm_str.push_str(&bb_name[1..]);
        asm_str.push_str(":\n"); 

        let insts = bb_node.insts();

        for (&inst, inst_node) in insts {
            value_to_asm(self, nm, inst, asm_str);
        }
    }
  }
}

pub fn value_to_asm(func_data: &FunctionData, nm: &mut NameManager, val: Value, asm_str: &mut String) -> String {
    let val_data = func_data.dfg().value(val); 
    let val_kind = val_data.kind();
    match val_kind {
        ValueKind::Return(ret) => {
            let ret_val = ret.value().unwrap();
            let ret_name = value_to_asm(func_data, nm, ret_val, asm_str);
            asm_str.push_str(&format!("   li a0, {}\n   ret", ret_name));
            String::new()
        },
        ValueKind::Integer(num) => num.value().to_string(),
        ValueKind::Binary(binary) => {
            let val_nm = nm.value_name(val_data).to_string();
            let val_name = format!("t{}", &val_nm[1..]);

            let lhs = binary.lhs();

            let lhs_data = func_data.dfg().value(lhs);
            let lhs_name = value_to_asm(func_data, nm, lhs, asm_str);

            let rhs = binary.rhs();
            let rhs_data = func_data.dfg().value(rhs);

            let rhs_name = value_to_asm(func_data, nm, rhs, asm_str);

            let inst_asm = match binary.op() {
                BinaryOp::Add => format!("add {} {} {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Sub => format!("sub {} {} {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Eq => format!("eq {} {} {}\n", val_name, lhs_name, rhs_name),
                _ => unimplemented!(),
            };
            asm_str.push_str(&inst_asm);
            val_name
        },
        _ => todo!("Value Kind Unimplement!"),
    }
}

pub fn program_to_asm(program: &Program) -> String {  
  let mut nm = NameManager::new();
  let mut asm_str = String::new();
  program.generate(&mut nm, &mut asm_str);
  asm_str
}