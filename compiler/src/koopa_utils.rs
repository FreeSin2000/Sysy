use koopa::ir::*;
use koopa::ir::layout::*;
use koopa::ir::builder_traits::*;
use koopa::ir::entities::*;

use koopa::back::KoopaGenerator;
use koopa::back::generator::NameManager;

use std::fmt::Write;

use std::cell::RefCell;
use std::collections::HashMap;

// 根据内存形式 Koopa IR 生成汇编
pub trait GenerateAsm {
  fn generate(&self, koopa_trans: &KoopaTrans, asm_str: &mut String);
}

impl GenerateAsm for Program {
  fn generate(&self, koopa_trans: &KoopaTrans, asm_str: &mut String) {
    asm_str.push_str("    .text\n");
    for &func in self.func_layout() {
        let func_name = self.func(func).name();
        asm_str.push_str(&format!("    .global {}\n", &func_name[1..]));
    }
    for &func in self.func_layout() {
        koopa_trans.enter_func_scope();
        self.func(func).generate(koopa_trans, asm_str);
        koopa_trans.exit_func_scope();
    }
  }
}

impl GenerateAsm for FunctionData {
  fn generate(&self, koopa_trans: &KoopaTrans, asm_str: &mut String) {
    let func_label = format!("{}:\n", &self.name()[1..]);
    asm_str.push_str(&func_label);

    // prologue
    let mut local_area_size = 0;
    for (_, bb_node) in self.layout().bbs() {
        let insts = bb_node.insts();
        for (&inst, _) in insts {
            let inst_ty = self.dfg().value(inst).ty();
            local_area_size += match inst_ty.kind() {
                TypeKind::Unit => 0,
                TypeKind::Int32 => 4,
                TypeKind::Pointer(_) => {
                    let mut vars_cnt = koopa_trans.vars_cnt.borrow_mut();
                    let mut vars_offset = koopa_trans.vars_offset.borrow_mut();
                    let inst_ptr: *const ValueData = self.dfg().value(inst);
                    vars_offset.insert(inst_ptr, *vars_cnt);
                    *vars_cnt += 1;
                    4
                },
                _ => unimplemented!("Unimplement value(insts)."),
            };
        }
    }
    local_area_size = (local_area_size + 15) / 16 * 16;
    assert!(local_area_size >= 0);
    assert!(local_area_size > -2048 && local_area_size < 2048);
    asm_str.push_str(&format!("    addi  sp, sp, {}\n", -local_area_size));

    for (bb, bb_node) in self.layout().bbs() {
        let bb_data = self.dfg().bb(*bb);
        let bb_name = &bb_data.name().clone().unwrap();
        asm_str.push_str(&bb_name[1..]);
        asm_str.push_str(":\n"); 

        let insts = bb_node.insts();

        for (&inst, _) in insts {
            asm_str.push_str(&koopa_trans.value_to_asm(self, inst));
        }
    }
    asm_str.push_str(&format!("    addi  sp, sp, {}\n    ret\n", local_area_size));
  }
}


pub struct KoopaTrans {
    nm: RefCell<NameManager>,
    vars_offset: RefCell<HashMap<*const ValueData, i32>>,
    vars_cnt: RefCell<i32>,
}
impl KoopaTrans {
    pub fn new() -> KoopaTrans {
        KoopaTrans {
            nm: RefCell::new(NameManager::new()),
            vars_offset: RefCell::new(HashMap::new()),
            vars_cnt: RefCell::new(0),
        }
    }
    pub fn vars_clear(&self) {
        let mut vars_offset = self.vars_offset.borrow_mut();
        vars_offset.clear();
        let mut vars_cnt = self.vars_cnt.borrow_mut();
        *vars_cnt = 0;
    }
    pub fn enter_func_scope(&self) {
        self.vars_clear();
        let mut nm = self.nm.borrow_mut();
        nm.enter_func_scope();
    }
    pub fn exit_func_scope(&self) {
        let mut nm = self.nm.borrow_mut();
        nm.exit_func_scope();
        self.vars_clear();
    }
    pub fn to_asm(&self, program: &Program) -> String {
        let mut asm_str = String::new();
        program.generate(self, &mut asm_str);
        asm_str
    }
    pub fn program_to_string(program: & Program) -> String {
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        return text_form_ir;
    }
    pub fn value_to_name(&self, func_data: &FunctionData, val: Value) -> String {
        let val_data = func_data.dfg().value(val);
        let mut nm = self.nm.borrow_mut();
        return nm.value_name(val_data).to_string()
    }
    pub fn value_to_offset(&self, func_data: &FunctionData, val: Value) -> i32 {
        let val_data = func_data.dfg().value(val);
        assert!(!val_data.kind().is_const());
        let val_ty = val_data.ty();
        match val_ty.kind() {
            TypeKind::Pointer(_) => {
                let vars_offset = self.vars_offset.borrow();
                let var_ptr: *const ValueData = val_data;
                *vars_offset.get(&var_ptr).unwrap() * 4
            },
            TypeKind::Int32 => {
                let val_name = self.value_to_name(func_data, val);
                (&val_name[1..].parse::<i32>().unwrap() + *self.vars_cnt.borrow()) * 4
            },
            _ => unimplemented!("Unimplement other value type(offset)."),
        }
    }

    pub fn value_to_asm(&self, func_data: &FunctionData, val: Value) -> String {
        let val_data = func_data.dfg().value(val); 
        let val_kind = val_data.kind();
        match val_kind {
            ValueKind::Return(ret) => {
                let ret_val = ret.value().unwrap();
                let ret_data = func_data.dfg().value(ret_val);

                if ret_data.kind().is_const() {
                    let ret_name = self.value_to_asm(func_data, ret_val); 
                    if ret_name == String::from("x0") {
                        format!("    mv    a0, {}\n", ret_name)
                    } else {
                        format!("    li    a0, {}\n", ret_name)
                    }
                } else {
                    let ret_offset = self.value_to_offset(func_data, ret_val);
                    format!("    lw    a0, {}(sp)\n", ret_offset)
                }
            },
            ValueKind::Integer(num) => {
                let num_val = num.value();
                if num_val == 0 {String::from("x0")} else {num_val.to_string()}
            },
            ValueKind::Binary(binary) => {
                let mut asm_str = String::new();

                let mut rs1 = String::from("t0");
                let mut rs2 = String::from("t1");
                let rd = String::from("t0");

                let lhs = binary.lhs();
                let lhs_asm = self.value_to_asm(func_data, lhs);
                let lhs_data = func_data.dfg().value(lhs);

                if lhs_data.kind().is_const() && lhs_asm != String::from("x0") {
                    asm_str = asm_str + &format!("    li    {}, {}\n", rs1, lhs_asm);
                } else {
                    if lhs_asm == String::from("x0") {
                        rs1 = String::from("x0");
                        rs2 = String::from("t0");
                    } else {
                        let lhs_offset = self.value_to_offset(func_data, lhs);
                        asm_str = asm_str + &format!("    lw    {}, {}(sp)\n", rs1, lhs_offset);
                    }
                }

                let rhs = binary.rhs();
                let rhs_asm = self.value_to_asm(func_data, rhs);
                let rhs_data = func_data.dfg().value(rhs);

                if rhs_data.kind().is_const() && rhs_asm != String::from("x0") {
                    asm_str = asm_str + &format!("    li    {}, {}\n", rs2, rhs_asm);
                } else {
                    if rhs_asm == String::from("x0") {
                        rs2 = String::from("x0");
                    } else {
                        let rhs_offset = self.value_to_offset(func_data, rhs);
                        asm_str = asm_str + &format!("    lw    {}, {}(sp)\n", rs2, rhs_offset);
                    }
                }
                let val_offset = self.value_to_offset(func_data, val);
                asm_str + &match binary.op() {
                    BinaryOp::Add => format!("    add   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Sub => format!("    sub   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Mul => format!("    mul   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Mod => format!("    rem   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Div => format!("    div   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::And => format!("    and   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Or => format!("    or    {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Lt => format!("    slt   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Gt => format!("    sgt   {}, {}, {}\n", rd, rs1, rs2),
                    BinaryOp::Le => {
                        format!("    sgt   {}, {}, {}\n    xori  {}, {}, {}\n", rd, rs1, rs2, rd, rd, 1)
                    },
                    BinaryOp::Ge => {
                        format!("    slt   {}, {}, {}\n    xori  {}, {}, {}\n", rd, rs1, rs2, rd, rd, 1)
                    },
                    BinaryOp::Eq => {
                        let xor_inst = format!("    xor   {}, {}, {}", rd, rs1, rs2); 
                        let seqz_inst = format!("    seqz  {}, {}", rd, rd); 
                        format!("{}\n{}\n", xor_inst, seqz_inst)
                    },
                    BinaryOp::NotEq => {
                        let xor_inst = format!("    xor   {}, {}, {}", rd, rs1, rs2); 
                        let seqz_inst = format!("    snez  {}, {}", rd, rd); 
                        format!("{}\n{}\n", xor_inst, seqz_inst)
                    },
                    _ => unimplemented!(),
                } + &format!("    sw    {}, {}(sp)\n", rd, val_offset)
            },
            ValueKind::Load(load_val) => {
                let rd = String::from("t0");
                let l_offset = self.value_to_offset(func_data, load_val.src());
                let s_offset = self.value_to_offset(func_data, val);
                println!("Load Ok\n");
                format!("    lw    {}, {}(sp)\n    sw    {}, {}(sp)\n", rd, l_offset, rd, s_offset)
            },
            ValueKind::Store(store_val) => {
                let s_val = store_val.value();
                let dest_val = store_val.dest();
                let mut rs1 = String::from("t0");
                let dest_offset = self.value_to_offset(func_data, dest_val);

                let pre_insts = if func_data.dfg().value(s_val).kind().is_const() {
                    let s_asm = self.value_to_asm(func_data, s_val);
                    if s_asm == String::from("x0") {
                        rs1 = String::from("x0");
                        String::new()
                    } else {
                        format!("    li    {}, {}\n", rs1, s_asm)
                    }
                } else {
                    let s_offset = self.value_to_offset(func_data, s_val);
                    format!("    lw    {}, {}(sp)\n", rs1, s_offset)
                };
                pre_insts + &format!("    sw    {}, {}(sp)\n", rs1, dest_offset)
            } ,
            ValueKind::Alloc(_) => String::from(""),
            _ => todo!("Value Kind Unimplement!"),
        }
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