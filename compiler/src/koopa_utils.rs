use koopa::ir::*;
use koopa::ir::layout::*;
use koopa::ir::builder_traits::*;
use koopa::ir::entities::*;
use koopa::ir::values::*;

use koopa::back::KoopaGenerator;
use koopa::back::generator::NameManager;

use std::fmt::Write;

use std::cell::RefCell;
use std::collections::HashMap;
use std::borrow::Borrow;

pub struct RegisterAllocator;

impl RegisterAllocator {
    pub fn get_ret_reg(&mut self) -> String {
        String::from("a0")
    } 
    pub fn get_binary_temps(&mut self) -> (String, String) {
        (String::from("t0"), String::from("t1"))
    }
    pub fn get_temp_reg(&mut self) -> String {
        String::from("t2")
    }
    pub fn get_cond_reg(&mut self) -> String {
        String::from("t3")
    }
    pub fn get_var_reg(&mut self) -> String {
        String::from("t4")
    }
}

pub struct FunctionAnalysis {
    pub stack_frame_size:i32,
    pub stack_value_offsets: HashMap<Value, i32>,
    pub is_leaf_call: bool,
}

impl FunctionAnalysis {
    pub fn new() -> Self {
        FunctionAnalysis {
            stack_frame_size: 0,
            stack_value_offsets: HashMap::new(),
            is_leaf_call: true,
        }
    } 
}

pub enum VarInfo {
    Int32(String),
}

pub struct KoopaTrans {
    reg_allocator: RegisterAllocator,
    func_analysis_cache: HashMap<String, FunctionAnalysis>,
    func_names: HashMap<Function, String>,
    global_vars: HashMap<Value, VarInfo>,
}

impl KoopaTrans {
    pub fn new() -> KoopaTrans {
        KoopaTrans {
            reg_allocator: RegisterAllocator,
            func_analysis_cache: HashMap::new(),
            func_names: HashMap::new(),
            global_vars: HashMap::new(),
        }
    }
    pub fn allocate_stack_slot(func_analysis: &mut FunctionAnalysis, value: Value, value_type: &Type) -> i32 {
        if let Some(offset) = func_analysis.stack_value_offsets.get(&value) {
            panic!("should not be here.");
            return *offset;
        }

        let size_bytes = match value_type.kind() {
            TypeKind::Int32 => 4,
            TypeKind::Pointer(_) => 4,
            _ => panic!("Attempted to allocate stack space for an invalid type"),
        };
        let cur_size = func_analysis.stack_frame_size;
        func_analysis.stack_value_offsets.insert(value, cur_size);
        func_analysis.stack_frame_size = cur_size + size_bytes;
        cur_size + size_bytes
    } 
    pub fn pre_analyze(&mut self, program: &Program) {
        for &func in program.func_layout() {
            let func_data = program.func(func);
            let opt_entry_bb = func_data.layout().entry_bb();
            let func_name = func_data.name().to_string();
            if let Some(_) = opt_entry_bb {
                let mut func_analysis = FunctionAnalysis::new();
                let mut cur_frame_size = 0;
                for (&bb, bb_node) in func_data.layout().bbs() {
                    let insts = bb_node.insts();
                    for (&inst, inst_node) in insts {
                        let inst_kind = func_data.dfg().value(inst).kind();
                        match inst_kind {
                            ValueKind::Call(call) => {
                                let num_params = call.args().len() as i32;
                                func_analysis.is_leaf_call = false;
                                let cur_params_size = (num_params - 8) * 4;
                                if cur_params_size > cur_frame_size { cur_frame_size = cur_params_size;}
                            },
                            _ => {continue;},
                        };
                    }
                }
                func_analysis.stack_frame_size = cur_frame_size;

                for (&bb, bb_node) in func_data.layout().bbs() {
                    let insts = bb_node.insts();
                    for (&inst, inst_node) in insts {
                        let inst_ty = func_data.dfg().value(inst).ty();
                        if !inst_ty.is_unit() {
                            // println!("{}: {:?}", func_name, func_data.dfg().value(inst));
                            cur_frame_size = Self::allocate_stack_slot(&mut func_analysis, inst, inst_ty);
                        } 
                    }
                }
                if !func_analysis.is_leaf_call {cur_frame_size += 4;}
                func_analysis.stack_frame_size = (cur_frame_size + 15) / 16 * 16;
                self.func_analysis_cache.insert(func_name.clone(), func_analysis);
            }
            self.func_names.insert(func, func_name.clone());
        }
    }

    pub fn generate_program(&mut self, program: &Program) -> String {
        let mut asm_str = String::from("");
        let mut asm_dir = String::from("");
        asm_dir.push_str("    .data\n");
        let global_vars = program.borrow_values();

        for (val, val_data) in global_vars.borrow().iter() {
            let val_kind = val_data.kind();
            // let val_ty = val_dta.ty();
            match val_kind {
                ValueKind::GlobalAlloc(global_alloc) => {
                    let val_name = val_data.name().clone().unwrap();
                    let init_val = global_alloc.init();
                    let init_data = program.borrow_value(init_val);

                    asm_dir.push_str(&format!("    .global {}\n", &val_name[1..]));
                    match init_data.kind() {
                        ValueKind::ZeroInit(_) => {
                            let var_size = 4;
                            asm_dir.push_str(&format!("{}:\n    .zero {}\n", &val_name[1..], var_size.to_string()));
                        },
                        ValueKind::Integer(int) => {
                            asm_dir.push_str(&format!("{}:\n    .word {}\n", &val_name[1..], int.value().to_string()));
                        },
                        _ => {panic!("Unimplementede globall alloc type.");},
                    };

                    self.global_vars.insert(*val, VarInfo::Int32(val_name[1..].to_string()));
                },
                _ => {continue;},
            }
        }

        asm_dir.push_str("    .text\n");
        for &func in program.func_layout() {
            let func_data = program.func(func);
            let opt_entry_bb = func_data.layout().entry_bb();
            if let Some(_) = opt_entry_bb {
                asm_str.push_str(&self.generate_func(func_data));
                let func_name = func_data.name().to_string();
                asm_dir.push_str(&format!("    .global {}\n", &func_name[1..]));
            }
        }
        asm_dir + &asm_str
    }

    pub fn generate_func(&mut self, func_data: &FunctionData) -> String {
        let mut asm_str = String::from("");
        let func_name = func_data.name().to_string();

        let func_analysis = self.func_analysis_cache.get(&func_name).unwrap();
        let stack_frame_size = func_analysis.stack_frame_size;
        let is_leaf_call = func_analysis.is_leaf_call;

        let func_label = format!("{}:\n", &func_name[1..]);
        asm_str.push_str(&func_label);

        // prologue
        asm_str.push_str(&format!("    addi  sp, sp, {}\n", -stack_frame_size));
        if !is_leaf_call {
            asm_str.push_str(&format!("    sw    ra, {}(sp)\n", stack_frame_size - 4));
        }
        for (bb, bb_node) in func_data.layout().bbs() {
            let bb_data = func_data.dfg().bb(*bb);
            let bb_name = &bb_data.name().clone().unwrap();
            asm_str.push_str(&bb_name[1..]);
            asm_str.push_str(":\n"); 

            let insts = bb_node.insts();

            for (&inst, _) in insts {
                asm_str.push_str(&self.generate_inst(inst, func_data));
            }
        }

        //epilogue
        asm_str.push_str(&format!("exit_{}:\n", &func_name[1..]));
        if !is_leaf_call {
            asm_str.push_str(&format!("    lw    ra, {}(sp)\n", stack_frame_size - 4));
        }
        asm_str.push_str(&format!("    addi  sp, sp, {}\n    ret\n", stack_frame_size));
        asm_str
    }

    pub fn generate_inst(&mut self, inst: Value, func_data: &FunctionData) -> String {
        let inst_data = func_data.dfg().value(inst);
        let inst_kind = inst_data.kind();
        match inst_kind {
            ValueKind::Return(ret) => {
                self.handle_ret(func_data, ret)
            },
            ValueKind::Binary(binary) => {
                self.handle_binary(func_data, binary, inst)
            },
            ValueKind::Store(store_val) => self.handle_store(func_data, store_val),
            ValueKind::Load(load_val) => self.handle_load(func_data, load_val, inst),
            ValueKind::Alloc(_) => String::from(""),
            ValueKind::Jump(jmp) => self.handle_jump(func_data, jmp),
            ValueKind::Branch(br) => self.handle_branch(func_data, br),
            ValueKind::Call(call) => {
                let opt_inst_val = if inst_data.ty().is_unit() {None} else {Some(inst)};
                self.handle_call(func_data, call, opt_inst_val)
            },
            _ => panic!("invalid inst"),
        }
    }

    pub fn handle_call(&mut self, func_data: &FunctionData, call: &Call, opt_des_val: Option<Value>) -> String {
        let mut call_asm = String::new();
        for (arg_id, arg_val) in call.args().iter().enumerate() {
            let temp_reg = self.reg_allocator.get_temp_reg();
            call_asm.push_str(&self.load_operand(func_data, *arg_val, &temp_reg));
            if arg_id >= 8 {
                let arg_offset = (arg_id as i32 - 8) * 4;
                call_asm.push_str(&format!("    sw    {}, {}(sp)\n", temp_reg, arg_offset.to_string()));
            } else {
                call_asm.push_str(&format!("    mv    a{}, {}\n", arg_id.to_string(), temp_reg));
            }
        }
        let func_name = self.func_names.get(&call.callee()).unwrap(); 
        call_asm.push_str(&format!("    call  {}\n", &func_name[1..]));
        if let Some(des_val) = opt_des_val {
            let des_reg = self.reg_allocator.get_ret_reg();
            call_asm.push_str(&self.store_operand(func_data, des_val, &des_reg));
        }
        call_asm
    }

    pub fn handle_store(&mut self, func_data: &FunctionData, store_val: &Store) -> String {
        let src_reg = self.reg_allocator.get_temp_reg();
        let mut store_asm = String::from("");
        store_asm.push_str(&self.load_operand(func_data, store_val.value(), &src_reg));
        store_asm.push_str(&self.store_operand(func_data, store_val.dest(), &src_reg));
        store_asm
    }
    pub fn handle_load(&mut self, func_data: &FunctionData, load_val: &Load, des_val: Value) -> String {
        let des_reg = self.reg_allocator.get_temp_reg();
        let mut load_asm = String::from("");
        load_asm.push_str(&self.load_operand(func_data, load_val.src(), &des_reg));
        load_asm.push_str(&self.store_operand(func_data, des_val, &des_reg));
        load_asm
    }
    pub fn handle_ret(&mut self, func_data: &FunctionData, ret: &Return) -> String {
        let opt_ret_val = ret.value();
        let mut ret_asm = String::from("");
        if let Some(ret_val) = opt_ret_val {
            let rd = self.reg_allocator.get_ret_reg();
            ret_asm.push_str(&self.load_operand(func_data, ret_val, &rd));
        }
        ret_asm.push_str(&format!("    j     exit_{}\n", &func_data.name()[1..]));
        ret_asm
    }
    pub fn get_bb_name(func_data: &FunctionData, bb: BasicBlock) -> String {
        func_data.dfg().bb(bb).name().clone().unwrap()
    }
    pub fn handle_branch(&mut self, func_data: &FunctionData, br: &Branch) -> String {
        let cond_reg = self.reg_allocator.get_cond_reg();
        let cond_val = br.cond();
        let then_bb = br.true_bb();
        let else_bb = br.false_bb();

        let then_name = Self::get_bb_name(func_data, then_bb);
        let else_name = Self::get_bb_name(func_data, else_bb);
        // let else_lable = func_data.dfg().bb(else_bb).name().clone().unwrap();
        let mut br_asm = String::from("");
        br_asm.push_str(&self.load_operand(func_data, cond_val, &cond_reg));
        br_asm.push_str(&format!("    bnez  {}, {}\n", cond_reg, &then_name[1..]));
        br_asm.push_str(&format!("    j     {}\n", &else_name[1..]));
        br_asm
    }
    pub fn handle_jump(&mut self, func_data: &FunctionData, jmp: &Jump) -> String {
        let tar_bb = jmp.target();
        // let tar_lable = func_data.dfg().bb(tar_bb).name().clone().unwrap();
        let tar_name = Self::get_bb_name(func_data, tar_bb);
        format!("    j     {}\n", &tar_name[1..])

    }
    pub fn handle_binary(&mut self, func_data: &FunctionData, binary: &Binary, des_val: Value) -> String {
        let (rs1, rs2) = self.reg_allocator.get_binary_temps();
        let rd = self.reg_allocator.get_temp_reg();
        let mut binary_asm = String::new();
        binary_asm.push_str(&self.load_operand(func_data, binary.lhs(), &rs1)); 
        binary_asm.push_str(&self.load_operand(func_data, binary.rhs(), &rs2)); 
        let binary_inst = match binary.op() {
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
        };
        binary_asm.push_str(&binary_inst);
        let store_inst = self.store_operand(func_data, des_val, &rd);
        binary_asm.push_str(&store_inst);
        binary_asm
    }
    pub fn get_stack_offset(&mut self, func_data: &FunctionData, val: Value) -> i32 {
        let func_name = func_data.name().to_string();
        let func_analysis = self.func_analysis_cache.get(&func_name).unwrap();
        *func_analysis.stack_value_offsets.get(&val).unwrap()
    } 
    pub fn get_stack_frame_size(&mut self, func_data: &FunctionData) -> i32 {
        let func_name = func_data.name().to_string();
        let func_analysis = self.func_analysis_cache.get(&func_name).unwrap();
        func_analysis.stack_frame_size
    }
    pub fn get_const(val_data: &ValueData) -> i32 {
        let val_kind = val_data.kind();
        assert!(val_kind.is_const());
        match val_kind {
            ValueKind::Integer(int) => int.value(),
            _ => panic!("Not a implemented const value."),
        }
    }
    pub fn load_operand(&mut self, func_data: &FunctionData, src_val: Value, des_reg: &String) -> String {
        if src_val.is_global() {
            let var_info = self.global_vars.get(&src_val).unwrap();
            match var_info {
                VarInfo::Int32(var_name) => {
                    format!("    la    {}, {}\n    lw    {}, 0({})\n", des_reg, var_name, des_reg, des_reg)
                },
                _ => {
                    panic!("Unimplemented global type.");
                },
            }

        } else {
            let src_data = func_data.dfg().value(src_val);
            if src_data.kind().is_const() {
                let src_num = Self::get_const(src_data);
                if src_num == 0 {
                    format!("    mv    {}, x0\n", des_reg)
                } else {
                    format!("    li    {}, {}\n", des_reg, src_num.to_string())
                }
            } else {
                match src_data.kind() {
                    ValueKind::FuncArgRef(func_arg_ref) => {
                        let arg_id = func_arg_ref.index() as i32;
                        if arg_id >= 8 {
                            let src_offset = (arg_id - 8) * 4 + self.get_stack_frame_size(func_data);
                            format!("    lw    {}, {}(sp)\n", des_reg, src_offset.to_string())
                        } else {
                            format!("    mv    {}, a{}\n", des_reg, arg_id.to_string())
                        }
                    },
                    _ => {
                        let src_offset = self.get_stack_offset(func_data, src_val);
                        format!("    lw    {}, {}(sp)\n", des_reg, src_offset.to_string())
                    }
                }
            }
        }
    }
    pub fn store_operand(&mut self, func_data: &FunctionData, des_val: Value, src_reg: &String) -> String {
        // let des_data = func_data.dfg().value(des_val);
        if des_val.is_global() {
            let var_info = self.global_vars.get(&des_val).unwrap();
            let var_reg = self.reg_allocator.get_var_reg();
            match var_info {
                VarInfo::Int32(var_name) => {
                    format!("    la    {}, {}\n    sw    {}, 0({})\n", var_reg, var_name, src_reg, var_reg)
                },
                _ => {
                    panic!("Unimplemented global type.");
                },
            }
        } else {
            let des_offset = self.get_stack_offset(func_data, des_val);
            format!("    sw    {}, {}(sp)\n", src_reg, des_offset.to_string())
        }
    }

    pub fn program_to_string(program: & Program) -> String {
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