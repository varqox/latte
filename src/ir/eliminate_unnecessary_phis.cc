#include "src/ir/eliminate_unnecessary_phis.hh"
#include "src/ir/ir.hh"
#include "src/overloaded.hh"

#include <algorithm>
#include <cassert>
#include <functional>
#include <map>
#include <optional>
#include <set>
#include <vector>

namespace ir {

// Returns an instruction to preppend instruction with if the phi is eliminated, otherwise
// returns std::nullopt
[[nodiscard]] static std::optional<Instruction> try_eliminate(
    const decltype(BasicBlock::phis)& phis, Var phi_var, const std::map<Label, Value>& preds) {
    // phi_var = phi [preds] can be safely eliminated only if every value in preds does not use
    // variable defined in phis other than phi_var
    for (auto const& [_, val] : preds) {
        bool can_eliminate_phi = std::visit(
            overloaded{
                [&](Var var) { return (var == phi_var or phis.find(var) == phis.end()); },
                [&](int_t /*unused*/) { return true; },
                [&](bool /*unused*/) { return true; },
                [&](Null /*unused*/) { return true; },
                [&](const StringConstantName& /*unused*/) { return true; },
                [&](const VTableName& /*unused*/) { return true; },
            },
            val);
        if (not can_eliminate_phi) {
            return std::nullopt;
        }
    }
    if (preds.empty()) {
        // This is OK to happen in a dead basic block, nothing can be done with just this phi,
        // so we just skip it
        return std::nullopt;
    }
    std::set<Value> vals;
    for (auto const& [_, val] : preds) {
        vals.emplace(val);
    }
    // All values from predecessors are equivalent
    if (vals.size() == 1) {
        return ICopy{
            .var = phi_var,
            .val = *vals.begin(),
        };
    }
    // All values from predecessors are itself or one other value, then we can set it to the
    // other value
    if (vals.size() == 2 and vals.find(Value{phi_var}) != vals.end()) {
        for (auto& val : vals) {
            if (val != Value{phi_var}) {
                return ICopy{
                    .var = phi_var,
                    .val = val,
                };
            }
        }
    }

    return std::nullopt;
}

[[nodiscard]] std::vector<Instruction>
eliminate_unnecessary_phis(decltype(BasicBlock::phis)& phis) {
    std::vector<Instruction> rev_preppend_instrs;
    auto it = phis.begin();
    while (it != phis.end()) {
        auto& [phi_var, preds] = *it;
        auto instr = try_eliminate(phis, phi_var, preds);
        if (instr) {
            rev_preppend_instrs.emplace_back(std::move(*instr));
            it = phis.erase(it);
        } else {
            ++it;
        }
    }
    std::reverse(rev_preppend_instrs.begin(), rev_preppend_instrs.end());
    return rev_preppend_instrs;
}

std::vector<BasicBlock> eliminate_unnecessary_phis(std::vector<BasicBlock>&& body) {
    for (auto& bblock : body) {
        auto pre_instructions = eliminate_unnecessary_phis(bblock.phis);
        for (auto& instr : bblock.instructions) {
            pre_instructions.emplace_back(std::move(instr));
        }
        bblock.instructions = std::move(pre_instructions);
    }
    return std::move(body);
}

} // namespace ir
