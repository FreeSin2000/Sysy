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
