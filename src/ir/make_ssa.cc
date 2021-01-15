#include "src/ir/make_ssa.hh"
#include "src/defs.hh"
#include "src/ir/ir.hh"
#include "src/ir/name_allocator.hh"
#include "src/ir/var_allocator.hh"
#include "src/overloaded.hh"

#include <cassert>
#include <map>
#include <optional>
#include <set>
#include <variant>
#include <vector>

namespace {

class SSAMaker {
    struct NewBasicBlock {
        ir::Label name;
        std::map<ir::Var, ir::Var> old_to_new_var;
        std::set<std::pair<ir::Var, ir::Var>> phis_to_make; // (phi var, undefined var)
        std::vector<ir::Instruction> new_instrs;

        std::set<ir::Label> predecessors;
        std::map<ir::Var, std::map<ir::Label, ir::Var>> phi_vars;
    };
    std::map<ir::Label, ir::Label> old_to_new_label;
    std::map<ir::Label, NewBasicBlock> bblocks;
    ir::VarAllocator var_allocator;

    ir::Var ssa_var(NewBasicBlock& nbb, ir::Var old_var) {
        auto it = nbb.old_to_new_var.find(old_var);
        if (it != nbb.old_to_new_var.end()) {
            return it->second;
        }
        auto new_var = var_allocator.alloc(old_var.type);
        auto [_, inserted] = nbb.old_to_new_var.emplace(old_var, new_var);
        assert(inserted);
        nbb.phis_to_make.emplace(new_var, old_var);
        return new_var;
    }

    ir::Var def_var(NewBasicBlock& nbb, ir::Var old_var) {
        auto new_var = var_allocator.alloc(old_var.type);
        nbb.old_to_new_var.insert_or_assign(old_var, new_var);
        return new_var;
    }

    ir::Value make_ssa(NewBasicBlock& nbb, ir::Value&& val) {
        return std::visit(
            overloaded{
                [&](ir::Var&& var) -> ir::Value { return ssa_var(nbb, var); },
                [&](int_t i) -> ir::Value { return i; },
                [&](bool b) -> ir::Value { return b; },
                [&](ir::Null n) -> ir::Value { return n; },
                [&](ir::StringConstantName&& scn) -> ir::Value { return scn; },
                [&](ir::VTableName&& vtn) -> ir::Value { return std::move(vtn); },
            },
            std::move(val));
    }

    ir::MemLoc make_ssa(NewBasicBlock& nbb, const ir::MemLoc& mloc) {
        return {
            .base = std::visit(
                overloaded{
                    [&](ir::Var var) -> decltype(mloc.base) { return ssa_var(nbb, var); },
                    [&](ir::Null n) -> decltype(mloc.base) { return n; },
                },
                mloc.base),
            .displacement = mloc.displacement,
            .scale = mloc.scale,
            .index = std::visit(
                overloaded{
                    [&](ir::Var var) -> decltype(mloc.index) { return ssa_var(nbb, var); },
                    [&](int_t i) -> decltype(mloc.index) { return i; },
                },
                mloc.index),
        };
    }

    ir::ConstMemLoc make_ssa(NewBasicBlock& nbb, const ir::ConstMemLoc& cmloc) {
        return {
            .loc = make_ssa(nbb, cmloc.loc),
        };
    }

    std::vector<ir::Value> make_ssa(NewBasicBlock& nbb, std::vector<ir::Value>&& vals) {
        for (auto& val : vals) {
            val = make_ssa(nbb, std::move(val));
        }
        return std::move(vals);
    }

    decltype(ir::ICall::func) make_ssa(NewBasicBlock& nbb, decltype(ir::ICall::func)&& fname) {
        return std::visit(
            overloaded{
                [&](ir::FnName&& fname) -> decltype(ir::ICall::func) {
                    return std::move(fname);
                },
                [&](ir::ConstMemLoc&& cmloc) -> decltype(ir::ICall::func) {
                    return make_ssa(nbb, cmloc);
                },
                [&](ir::Value&& val) -> decltype(ir::ICall::func) {
                    return make_ssa(nbb, std::move(val));
                },
            },
            std::move(fname));
    }

    void fill_new_bblock(NewBasicBlock& nbb, ir::BasicBlock&& bblock) {
        auto new_label = [&](ir::Label old_label) {
            auto label = old_to_new_label.at(old_label);
            bblocks.at(label).predecessors.emplace(nbb.name);
            return label;
        };
        for (auto& instr : bblock.instructions) {
            nbb.new_instrs.emplace_back(std::visit(
                overloaded{
                    [&](ir::ICopy&& i) -> ir::Instruction {
                        auto ssa_val = make_ssa(nbb, std::move(i.val));
                        return ir::ICopy{
                            .var = def_var(nbb, i.var),
                            .val = std::move(ssa_val),
                        };
                    },
                    [&](ir::IUnaryOp&& i) -> ir::Instruction {
                        auto ssa_val = make_ssa(nbb, std::move(i.val));
                        return ir::IUnaryOp{
                            .var = def_var(nbb, i.var),
                            .op = i.op,
                            .val = std::move(ssa_val),
                        };
                    },
                    [&](ir::IBinOp&& i) -> ir::Instruction {
                        auto ssa_left = make_ssa(nbb, std::move(i.left));
                        auto ssa_right = make_ssa(nbb, std::move(i.right));
                        return ir::IBinOp{
                            .var = def_var(nbb, i.var),
                            .op = i.op,
                            .left = std::move(ssa_left),
                            .right = std::move(ssa_right),
                        };
                    },
                    [&](ir::ILoad&& i) -> ir::Instruction {
                        auto ssa_loc = make_ssa(nbb, i.loc);
                        return ir::ILoad{
                            .var = def_var(nbb, i.var),
                            .loc = ssa_loc,
                        };
                    },
                    [&](ir::IConstLoad&& i) -> ir::Instruction {
                        auto ssa_loc = make_ssa(nbb, i.loc);
                        return ir::IConstLoad{
                            .var = def_var(nbb, i.var),
                            .loc = ssa_loc,
                        };
                    },
                    [&](ir::IStore&& i) -> ir::Instruction {
                        return ir::IStore{
                            .loc = make_ssa(nbb, i.loc),
                            .val = make_ssa(nbb, std::move(i.val)),
                        };
                    },
                    [&](ir::ICall&& i) -> ir::Instruction {
                        auto ssa_func = make_ssa(nbb, std::move(i.func));
                        auto ssa_args = make_ssa(nbb, std::move(i.args));
                        return ir::ICall{
                            .var = def_var(nbb, i.var),
                            .func = std::move(ssa_func),
                            .args = std::move(ssa_args),
                        };
                    },
                    [&](ir::IVCall&& i) -> ir::Instruction {
                        return ir::IVCall{
                            .func = make_ssa(nbb, std::move(i.func)),
                            .args = make_ssa(nbb, std::move(i.args)),
                        };
                    },
                    [&](ir::IGoto&& i) -> ir::Instruction {
                        return ir::IGoto{
                            .target = new_label(i.target),
                        };
                    },
                    [&](ir::IIfUnaryCond&& i) -> ir::Instruction {
                        return ir::IIfUnaryCond{
                            .negate_cond = i.negate_cond,
                            .cond = ssa_var(nbb, i.cond),
                            .true_branch = new_label(i.true_branch),
                            .false_branch = new_label(i.false_branch),
                        };
                    },
                    [&](ir::IIfBinCond&& i) -> ir::Instruction {
                        return ir::IIfBinCond{
                            .op = i.op,
                            .left = make_ssa(nbb, std::move(i.left)),
                            .right = make_ssa(nbb, std::move(i.right)),
                            .true_branch = new_label(i.true_branch),
                            .false_branch = new_label(i.false_branch),
                        };
                    },
                    [&](ir::IReturn&& i) -> ir::Instruction {
                        return ir::IReturn{
                            .val = i.val ? std::optional{make_ssa(nbb, std::move(*i.val))}
                                         : std::nullopt,
                        };
                    },
                    [&](ir::IUnreachable&& /*unused*/) -> ir::Instruction {
                        return ir::IUnreachable{};
                    },
                },
                std::move(instr)));
        }
    }

    ir::Var ssa_var_from(NewBasicBlock& nbb, ir::Var old_var) {
        auto it = nbb.old_to_new_var.find(old_var);
        if (it != nbb.old_to_new_var.end()) {
            return it->second;
        }
        // There is no definition of the old_var in the nbb, we need to create a phi for it
        auto phi_var = var_allocator.alloc(old_var.type);
        auto [_, inserted] = nbb.old_to_new_var.try_emplace(old_var, phi_var);
        assert(inserted);
        fill_phi(nbb, phi_var, old_var);
        return phi_var;
    }

    void fill_phi(NewBasicBlock& nbb, ir::Var phi_var, ir::Var old_var) {
        auto [it, inserted] = nbb.phi_vars.try_emplace(phi_var);
        assert(inserted);
        auto& phi_predecesors = it->second;
        for (auto pred : nbb.predecessors) {
            phi_predecesors.try_emplace(pred, ssa_var_from(bblocks.at(pred), old_var));
        }
    }

public:
    SSAMaker() = default;

    ir::FnDef make_ssa(ir::FnDef&& fdef) {
        NameAllocator<ir::Label> label_allocator;
        decltype(NewBasicBlock::old_to_new_var) old_arg_to_new_arg;
        for (auto old_arg : fdef.args) {
            old_arg_to_new_arg.try_emplace(old_arg, var_allocator.alloc(old_arg.type));
        }
        // Fill old_to_new_label and bblocks
        for (auto const& bblock : fdef.body) {
            auto new_name = label_allocator.alloc();
            old_to_new_label.try_emplace(bblock.name, new_name);
            bblocks.try_emplace(
                new_name,
                NewBasicBlock{
                    .name = new_name,
                    .old_to_new_var = old_arg_to_new_arg,
                    .phis_to_make = {},
                    .new_instrs = {},
                    .predecessors = {},
                    .phi_vars = {},
                });
            old_arg_to_new_arg = {}; // only first block has args given, others have to use phi
        }
        // Make each BasicBlock SSA
        for (auto& bblock : fdef.body) {
            auto& new_name = old_to_new_label.at(bblock.name);
            fill_new_bblock(bblocks.at(new_name), std::move(bblock));
        }
        // Fill phis
        for (auto& [_, nbb] : bblocks) {
            for (auto [phi_var, undef_old_var] : nbb.phis_to_make) {
                fill_phi(nbb, phi_var, undef_old_var);
            }
        }
        return {
            .name = fdef.name,
            .ret_type = fdef.ret_type,
            .args = fdef.args,
            .body =
                [this] {
                    std::vector<ir::BasicBlock> res_bblocks;
                    for (auto& [name, nbb] : bblocks) {
                        res_bblocks.emplace_back(ir::BasicBlock{
                            .name = name,
                            .phis =
                                [&, &nbb = nbb] {
                                    decltype(ir::BasicBlock::phis) phis;
                                    for (auto& [var, phi_var_info] : nbb.phi_vars) {
                                        phis.try_emplace(
                                            var, [&, &phi_var_info = phi_var_info] {
                                                std::map<ir::Label, ir::Value> res;
                                                for (auto const& [label, pred_var] :
                                                     phi_var_info) {
                                                    res.emplace(label, pred_var);
                                                }
                                                return res;
                                            }());
                                    }
                                    return phis;
                                }(),
                            .instructions = std::move(nbb.new_instrs),
                        });
                    }
                    return res_bblocks;
                }(),
        };
    }
};

} // namespace

namespace ir {

static std::vector<FnDef> make_ssa(std::vector<FnDef>&& fdefs) {
    std::vector<FnDef> res;
    res.reserve(fdefs.size());
    for (auto& fdef : fdefs) {
        res.emplace_back(SSAMaker{}.make_ssa(std::move(fdef)));
    }
    return res;
}

Program make_ssa(Program&& prog) {
    return {
        .strings = prog.strings,
        .vtables = prog.vtables,
        .functions = make_ssa(std::move(prog.functions)),
    };
}

} // namespace ir
