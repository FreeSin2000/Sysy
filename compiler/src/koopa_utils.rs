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
    asm_str.push_str("    .text\n");
    for &func in self.func_layout() {
        let func_name = self.func(func).name();
        asm_str.push_str(&format!("    .global {}\n", &func_name[1..]));
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
            asm_str.push_str(&value_to_asm(self, nm, inst));
        }
    }
  }
}
pub fn value_to_name(func_data: &FunctionData, nm: &mut NameManager, val: Value) -> String {
    let val_data = func_data.dfg().value(val); 
    if val_data.kind().is_const() { 
        value_to_asm(func_data, nm, val)
    } else {
        format!("t{}", &nm.value_name(val_data).to_string()[1..])
    }
}
pub fn value_to_asm(func_data: &FunctionData, nm: &mut NameManager, val: Value) -> String {
    let val_data = func_data.dfg().value(val); 
    let val_kind = val_data.kind();
    match val_kind {
        ValueKind::Return(ret) => {
            let ret_val = ret.value().unwrap();
            let ret_data = func_data.dfg().value(ret_val);
            let ret_name = value_to_name(func_data, nm, ret_val);

            if ret_data.kind().is_const() && ret_name != "x0" {
                format!("    li    a0, {}\n    ret", ret_name)
            } else {
                let ret_name = value_to_name(func_data, nm, ret_val);
                format!("    mv    a0, {}\n    ret", ret_name)
            }
        },
        ValueKind::Integer(num) => {
            let num_val = num.value();
            if num_val == 0 {String::from("x0")} else {num_val.to_string()}
        },
        ValueKind::Binary(binary) => {
            let mut asm_str = String::new();

            let val_nm = nm.value_name(val_data).to_string();
            let val_name = format!("t{}", &val_nm[1..]);

            let lhs = binary.lhs();
            let mut lhs_name = value_to_name(func_data, nm, lhs);
            let lhs_data = func_data.dfg().value(lhs);
            if lhs_data.kind().is_const() && lhs_name != String::from("x0") {
                asm_str = asm_str + &format!("    li    {}, {}\n", val_name, lhs_name);
                lhs_name = val_name.clone();
            }
            let rhs = binary.rhs();
            let mut rhs_name = value_to_name(func_data, nm, rhs);
            let rhs_data = func_data.dfg().value(rhs);

            if rhs_data.kind().is_const() && rhs_name != String::from("x0") {
                if lhs_name != val_name {
                    asm_str = asm_str + &format!("    li    {}, {}\n", val_name, rhs_name);
                    rhs_name = val_name.clone();
                } else {
                    asm_str = asm_str + &format!("    li    a0, {}\n", rhs_name);
                    rhs_name = String::from("a0");
                }
            }
            asm_str + &match binary.op() {
                BinaryOp::Add => format!("    add   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Sub => format!("    sub   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Eq => {
                    let xor_inst = format!("    xor   {}, {}, {}", val_name, val_name, rhs_name); 
                    let seqz_inst = format!("    seqz  {}, {}", val_name, val_name); 
                    format!("{}\n{}\n", xor_inst, seqz_inst)
                },
                _ => unimplemented!(),
            }
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