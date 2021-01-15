#include "src/ir/propagate_copies.hh"
#include "src/ir/ir.hh"

#include <map>
#include <vector>

namespace ir {

std::vector<BasicBlock> propagate_copies(std::vector<BasicBlock>&& body) {
    std::map<Var, Var> var_copy_map;
    // Fill var_copy_map
    for (auto const& bblock : body) {
        for (auto const& instr : bblock.instructions) {
            auto* icopy = std::get_if<ICopy>(&instr);
            if (icopy) {
                if (auto* val_var = std::get_if<Var>(&icopy->val)) {
                    var_copy_map.emplace(icopy->var, *val_var);
                }
            }
        }
    }
    // Propagate copies
    auto reduce_var_impl = [&](auto& self, Var v) {
        auto it = var_copy_map.find(v);
        if (it == var_copy_map.end() or it->second == v) {
            return v;
        }
        return it->second = self(self, it->second);
    };
    auto reduce_var = [&](Var& v) { v = reduce_var_impl(reduce_var_impl, v); };
    auto reduce_mloc = [&](MemLoc& mloc) {
        std::visit(
            overloaded{
                [&](Var& v) { reduce_var(v); },
                [&](Null& /*unused*/) {},
            },
            mloc.base);
        std::visit(
            overloaded{
                [&](Var& v) { reduce_var(v); },
                [&](int_t& /*unused*/) {},
            },
            mloc.index);
    };
    auto reduce_cmloc = [&](ConstMemLoc& cmloc) { reduce_mloc(cmloc.loc); };
    auto reduce_val = [&](Value& val) {
        std::visit(
            overloaded{
                [&](Var& v) { reduce_var(v); },
                [&](int_t& /*unused*/) {},
                [&](bool& /*unused*/) {},
                [&](Null& /*unused*/) {},
                [&](StringConstantName& /*unused*/) {},
                [&](VTableName& /*unused*/) {},
            },
            val);
    };
    auto reduce_func = [&](decltype(ICall::func)& func) {
        std::visit(
            overloaded{
                [&](FnName& /*unused*/) {},
                [&](ConstMemLoc& cmloc) { reduce_cmloc(cmloc); },
                [&](Value& val) { reduce_val(val); },
            },
            func);
    };
    auto reduce_instr = [&](Instruction& instr) {
        std::visit(
            overloaded{
                [&](ICopy& i) { reduce_val(i.val); },
                [&](IUnaryOp& i) { reduce_val(i.val); },
                [&](IBinOp& i) {
                    reduce_val(i.left);
                    reduce_val(i.right);
                },
                [&](ILoad& i) { reduce_mloc(i.loc); },
                [&](IConstLoad& i) { reduce_cmloc(i.loc); },
                [&](IStore& i) {
                    reduce_mloc(i.loc);
                    reduce_val(i.val);
                },
                [&](ICall& i) {
                    reduce_func(i.func);
                    for (auto& arg : i.args) {
                        reduce_val(arg);
                    }
                },
                [&](IVCall& i) {
                    reduce_func(i.func);
                    for (auto& arg : i.args) {
                        reduce_val(arg);
                    }
                },
                [&](IGoto& /*unused*/) {},
                [&](IIfUnaryCond& i) { reduce_var(i.cond); },
                [&](IIfBinCond& i) {
                    reduce_val(i.left);
                    reduce_val(i.right);
                },
                [&](IReturn& i) {
                    if (i.val) {
                        reduce_val(*i.val);
                    }
                },
                [&](IUnreachable& /*unused*/) {},
            },
            instr);
    };
    for (auto& bblock : body) {
        for (auto& instr : bblock.instructions) {
            reduce_instr(instr);
        }
        for (auto& [phi_var, preds] : bblock.phis) {
            for (auto& [_, val] : preds) {
                reduce_val(val);
            }
        }
    }
    return std::move(body);
}

} // namespace ir
