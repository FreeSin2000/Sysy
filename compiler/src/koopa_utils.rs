use koopa::ir::*;
use koopa::ir::layout::*;
use koopa::ir::builder_traits::*;
use std::fmt::Write;

pub fn program_to_string(program: & Program) -> String {
    let mut res: String = String::new();
    for func in program.func_layout() {
        let func_data = program.func(*func);
        let func_name = &func_data.name();

        let func_ty = match func_data.ty().kind() {
            TypeKind::Function(_, ret_ty) => match ret_ty.kind() {
                TypeKind::Int32 => "i32",
                TypeKind::Unit => "",
                _ => todo!("Unimplement!"),
            },
            _ => todo!("Illigel function type!"),
        };
        res = res + &format!("fun {}(): {} {{\n{}\n}}", func_name, func_ty, bbs_to_string(&func_data));
    }
    return res;
}

pub fn value_to_string(func_data: &FunctionData, val: &Value) -> String {
    let val_data = func_data.dfg().value(*val); 
    let val_kind = val_data.kind();
    match val_kind {
        ValueKind::Return(ret) => {
            let ret_val = ret.value().unwrap();
            String::from("  ret ") + &value_to_string(func_data, &ret_val)
        },
        ValueKind::Integer(num) => num.value().to_string(),
        _ => todo!("Value Kind Unimplement!"),
    }
}

pub fn bbs_to_string(func_data: &FunctionData) -> String {
    let mut bb_res = String::new();

    for (bb, bb_node) in func_data.layout().bbs() {
        let bb_data = func_data.dfg().bb(*bb);
        let bb_name = &bb_data.name().clone().unwrap();
        bb_res.push_str(bb_name);
        bb_res.push_str(":\n"); 

        let insts = bb_node.insts();

        for (inst, inst_node) in insts {
            let inst_str = value_to_string(func_data, inst);
            bb_res.push_str(&inst_str);
        }
    }
    bb_res
}

// 根据内存形式 Koopa IR 生成汇编
pub trait GenerateAsm {
  fn generate(&self, asm_str: &mut String);
}

impl GenerateAsm for Program {
  fn generate(&self, asm_str: &mut String) {
    asm_str.push_str("  .text\n");
    for &func in self.func_layout() {
        let func_name = self.func(func).name();
        asm_str.push_str(&format!("  .global {}\n", &func_name[1..]));
    }
    for &func in self.func_layout() {
      self.func(func).generate(asm_str);
    }
  }
}

impl GenerateAsm for FunctionData {
  fn generate(&self, asm_str: &mut String) {
    let func_label = format!("{}:\n", &self.name()[1..]);
    asm_str.push_str(&func_label);
    let mut bb_asm = String::new();

    for (bb, bb_node) in self.layout().bbs() {
        let bb_data = self.dfg().bb(*bb);
        let bb_name = &bb_data.name().clone().unwrap();
        bb_asm.push_str(&bb_name[1..]);
        bb_asm.push_str(":\n"); 

        let insts = bb_node.insts();

        for (inst, inst_node) in insts {
            let inst_asm = value_to_asm(self, inst);
            bb_asm.push_str(&inst_asm);
        }
    }
    asm_str.push_str(&bb_asm);
  }
}

pub fn value_to_asm(func_data: &FunctionData, val: &Value) -> String {
    let val_data = func_data.dfg().value(*val); 
    let val_kind = val_data.kind();
    match val_kind {
        ValueKind::Return(ret) => {
            let ret_val = ret.value().unwrap();
            let ret_val_asm = value_to_asm(func_data, &ret_val);
            format!("   li a0, {}\n   ret", ret_val_asm)
        },
        ValueKind::Integer(num) => num.value().to_string(),
        _ => todo!("Value Kind Unimplement!"),
    }
}