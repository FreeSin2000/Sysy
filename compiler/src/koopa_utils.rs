use koopa::ir::*;
use koopa::ir::layout::*;
use koopa::ir::builder_traits::*;

use koopa::back::KoopaGenerator;
use koopa::back::generator::NameManager;

use std::fmt::Write;

use std::cell::RefCell;

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
        // format!("t{}", &nm.value_name(val_data).to_string()[1..])
        idx_to_reg(nm.value_name(val_data).to_string()[1..].parse::<usize>().unwrap())
    }
}
pub fn idx_to_reg(idx: usize) -> String {
    assert!(idx < 15);
    if idx < 7 {
        format!("t{}", idx.to_string())
    } else {
        format!("a{}", (idx - 7).to_string())
    }
}
pub fn reg_to_idx(reg_name: &str) -> Option<usize> {
    match reg_name {
        // T 寄存器 (t0-t6): 7个，序号从 0 开始
        "t0" => Some(0),
        "t1" => Some(1),
        "t2" => Some(2),
        "t3" => Some(3),
        "t4" => Some(4),
        "t5" => Some(5),
        "t6" => Some(6),
        
        // A 寄存器 (a0-a7): 8个，接在 T 寄存器后面
        "a0" => Some(7),
        "a1" => Some(8),
        "a2" => Some(9),
        "a3" => Some(10),
        "a4" => Some(11),
        "a5" => Some(12),
        "a6" => Some(13),
        "a7" => Some(14),
        
        // 如果输入了其他名称，返回 None
        _ => None,
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
            let val_name = idx_to_reg(val_nm[1..].parse::<usize>().unwrap());

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
                    let val_idx = reg_to_idx(&val_name).unwrap();
                    let next_reg = idx_to_reg(val_idx + 1);
                    asm_str = asm_str + &format!("    li    {}, {}\n", next_reg, rhs_name);
                    rhs_name = next_reg;
                }
            }
            asm_str + &match binary.op() {
                BinaryOp::Add => format!("    add   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Sub => format!("    sub   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Mul => format!("    mul   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Mod => format!("    rem   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Div => format!("    div   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::And => format!("    and   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Or => format!("    or    {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Lt => format!("    slt   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Gt => format!("    sgt   {}, {}, {}\n", val_name, lhs_name, rhs_name),
                BinaryOp::Le => {
                    format!("    sgt   {}, {}, {}\n    xori  {}, {}, {}\n", val_name, lhs_name, rhs_name, val_name, val_name, 1)
                },
                BinaryOp::Ge => {
                    format!("    slt   {}, {}, {}\n    xori  {}, {}, {}\n", val_name, lhs_name, rhs_name, val_name, val_name, 1)
                },
                BinaryOp::Eq => {
                    let xor_inst = format!("    xor   {}, {}, {}", val_name, lhs_name, rhs_name); 
                    let seqz_inst = format!("    seqz  {}, {}", val_name, val_name); 
                    format!("{}\n{}\n", xor_inst, seqz_inst)
                },
                BinaryOp::NotEq => {
                    let xor_inst = format!("    xor   {}, {}, {}", val_name, lhs_name, rhs_name); 
                    let seqz_inst = format!("    snez  {}, {}", val_name, val_name); 
                    format!("{}\n{}\n", xor_inst, seqz_inst)
                },
                _ => unimplemented!(),
            }
        },
        _ => todo!("Value Kind Unimplement!"),
    }
}

pub struct KoopaTrans {
    nm: RefCell<NameManager>,
}
impl KoopaTrans {
    pub fn new() -> KoopaTrans {
        KoopaTrans {nm: RefCell::new(NameManager::new())}
    }
    pub fn to_asm(&self, program: &Program) -> String {
        let mut asm_str = String::new();
        program.generate(&mut self.nm.borrow_mut(), &mut asm_str);
        asm_str
    }
    pub fn to_string(program: & Program) -> String {
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        return text_form_ir;
    }
}

// pub fn program_to_asm(program: &Program) -> String {  
//   let mut nm = NameManager::new();
//   let mut asm_str = String::new();
//   program.generate(&mut nm, &mut asm_str);
//   asm_str
// }

// pub fn koopa_to_string(program: & Program) -> String {
//     let mut gen = KoopaGenerator::new(Vec::new());
//     gen.generate_on(program).unwrap();
//     let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
//     return text_form_ir;
// }