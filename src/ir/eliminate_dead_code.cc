#include "src/ir/eliminate_dead_code.hh"
#include "src/ir/bblock_pred_succ_info.hh"
#include "src/ir/ir.hh"
#include "src/overloaded.hh"

#include <cassert>
#include <map>
#include <set>
#include <variant>
#include <vector>

namespace ir {

static std::vector<BasicBlock> eliminate_dead_blocks(std::vector<BasicBlock>&& body) {
    auto bbinfos = bblocks_pred_succ_info(body);
    // Mark all blocks that are reachable from start
    std::set<ir::Label> visited;
    std::vector<ir::Label> stack = {body.front().name};
    while (not stack.empty()) {
        auto x = stack.back();
        stack.pop_back();
        if (not visited.emplace(x).second) {
            continue;
        }
        for (auto y : bbinfos.at(x).successors) {
            stack.emplace_back(y);
        }
    }
    // Remove dead nodes (as predecessors) from its successors
    for (auto const& bblock : body) {
        if (visited.count(bblock.name)) {
            continue; // alive
        }
        // dead
        for (auto succ_name : bbinfos.at(bblock.name).successors) {
            // Remove dead bblock from phis of its successors
            for (auto&& [var, preds] : bbinfos.at(succ_name).bblock->phis) {
                preds.erase(bblock.name);
            }
        }
    }
    // Remove all dead blocks
    std::vector<BasicBlock> res;
    for (auto& bblock : body) {
        if (visited.count(bblock.name)) {
            res.emplace_back(std::move(bblock));
        }
    }
    return res;
}

static void eliminate_dead_variables(std::vector<BasicBlock>& body) {
    std::set<Var> used_variables;
    auto process_mloc = [&](const MemLoc& mloc) {
        std::visit(
            overloaded{
                [&](Var v) { used_variables.emplace(v); },
                [&](Null /*unused*/) {},
            },
            mloc.base);
        std::visit(
            overloaded{
                [&](Var v) { used_variables.emplace(v); },
                [&](int_t /*unused*/) {},
            },
            mloc.index);
    };
    auto process_cmloc = [&](const ConstMemLoc& cmloc) { process_mloc(cmloc.loc); };
    auto process_val = [&](const Value& val) {
        std::visit(
            overloaded{
                [&](Var v) { used_variables.emplace(v); },
                [&](int_t /*unused*/) {},
                [&](bool /*unused*/) {},
                [&](Null /*unused*/) {},
                [&](const StringConstantName& /*unused*/) {},
                [&](const VTableName& /*unused*/) {},
            },
            val);
    };
    auto process_func = [&](const decltype(ICall::func)& func) {
        std::visit(
            overloaded{
                [&](const FnName& /*unused*/) {},
                [&](const ConstMemLoc& cmloc) { process_cmloc(cmloc); },
                [&](const Value& val) { process_val(val); },
            },
            func);
    };
    auto process_instr = [&](const Instruction& instr) {
        std::visit(
            overloaded{
                [&](const ICopy& i) { process_val(i.val); },
                [&](const IUnaryOp& i) { process_val(i.val); },
                [&](const IBinOp& i) {
                    process_val(i.left);
                    process_val(i.right);
                },
                [&](const ILoad& i) { process_mloc(i.loc); },
                [&](const IConstLoad& i) { process_cmloc(i.loc); },
                [&](const IStore& i) {
                    process_mloc(i.loc);
                    process_val(i.val);
                },
                [&](const ICall& i) {
                    process_func(i.func);
                    for (auto const& arg : i.args) {
                        process_val(arg);
                    }
                },
                [&](const IVCall& i) {
                    process_func(i.func);
                    for (auto const& arg : i.args) {
                        process_val(arg);
                    }
                },
                [&](const IGoto& /*unused*/) {},
                [&](const IIfUnaryCond& i) { process_val(i.cond); },
                [&](const IIfBinCond& i) {
                    process_val(i.left);
                    process_val(i.right);
                },
                [&](const IReturn& i) {
                    if (i.val) {
                        process_val(*i.val);
                    }
                },
                [&](const IUnreachable& /*unused*/) {},
            },
            instr);
    };
    // Fill used_variables
    for (auto const& bblock : body) {
        for (auto const& instr : bblock.instructions) {
            process_instr(instr);
        }
        for (auto const& [phi_var, preds] : bblock.phis) {
            for (auto const& [_, val] : preds) {
                process_val(val);
            }
        }
    }
    // Get rid of unused variables
    auto reduce = [&](Instruction&& instr) -> std::optional<Instruction> {
        return std::visit(
            overloaded{
                [&](ICopy&& i) -> std::optional<Instruction> {
                    if (used_variables.count(i.var)) {
                        return std::move(i);
                    }
                    return std::nullopt;
                },
                [&](IUnaryOp&& i) -> std::optional<Instruction> {
                    if (used_variables.count(i.var)) {
                        return std::move(i);
                    }
                    return std::nullopt;
                },
                [&](IBinOp&& i) -> std::optional<Instruction> {
                    if (used_variables.count(i.var)) {
                        return std::move(i);
                    }
                    return std::nullopt;
                },
                [&](ILoad&& i) -> std::optional<Instruction> {
                    if (used_variables.count(i.var)) {
                        return i;
                    }
                    return std::nullopt;
                },
                [&](IConstLoad&& i) -> std::optional<Instruction> {
                    if (used_variables.count(i.var)) {
                        return i;
                    }
                    return std::nullopt;
                },
                [&](IStore&& i) -> std::optional<Instruction> { return std::move(i); },
                [&](ICall&& i) -> std::optional<Instruction> {
                    if (used_variables.count(i.var)) {
                        return std::move(i);
                    }
                    return IVCall{
                        .func = std::move(i.func),
                        .args = std::move(i.args),
                    };
                },
                [&](IVCall&& i) -> std::optional<Instruction> { return std::move(i); },
                [&](IGoto&& i) -> std::optional<Instruction> { return i; },
                [&](IIfUnaryCond&& i) -> std::optional<Instruction> { return i; },
                [&](IIfBinCond&& i) -> std::optional<Instruction> { return std::move(i); },
                [&](IReturn&& i) -> std::optional<Instruction> { return std::move(i); },
                [&](IUnreachable&& i) -> std::optional<Instruction> { return i; },
            },
            std::move(instr));
    };
    for (auto& bblock : body) {
        // Instructions
        std::vector<Instruction> new_instrs;
        for (auto& instr : bblock.instructions) {
            auto new_instr = reduce(std::move(instr));
            if (new_instr) {
                new_instrs.emplace_back(std::move(*new_instr));
            }
        }
        bblock.instructions = std::move(new_instrs);
        // Phis
        auto it = bblock.phis.begin();
        while (it != bblock.phis.end()) {
            if (used_variables.count(it->first)) {
                ++it;
            } else {
                it = bblock.phis.erase(it);
            }
        }
    }
}

std::vector<BasicBlock> eliminate_redundant_gotos(std::vector<BasicBlock>&& body) {
    auto bbinfos = bblocks_pred_succ_info(body);
    // E.g. if we have
    // .L1:
    //     ...
    //     goto .L2
    // .L2:
    //     ...
    //     goto .L3
    // .L3:
    //    ...
    //     ; here some jump
    // ...
    // .L28:
    //      v42 = phi [.L3: v8, .L13: v32]
    // and we want to merge L1, L2 and L3, we need to replace the label .L3 with .L1 in
    // phi of v42.
    std::set<Label> merged_bblocks;
    for (auto& bblock : body) {
        // Looking at the above example, we want to do the merging only if the bblock is the
        // first block of the merged blocks, so that we won't move instructions around in
        // suboptimal order, so we merge .L2 to .L1, then .L3 to .L1, not .L3 to .L2 and then
        // .L2 to .L1.
        auto curr_bbi = bbinfos.at(bblock.name);
        if (bblock.phis.empty() and curr_bbi.predecessors.size() == 1 and
            std::holds_alternative<IGoto>(
                bbinfos.at(*curr_bbi.predecessors.begin()).bblock->instructions.back()))
        {
            // This block is suitable to *BE* merged to predecessor, so we will merge it and
            // the potentially viable successor from the predecessor
            continue;
        }
        while (auto* igoto = std::get_if<IGoto>(&bblock.instructions.back())) {
            auto target_name = igoto->target;
            auto& target_bbi = bbinfos.at(target_name);
            if (target_bbi.predecessors.size() != 1) {
                break; // cannot merge, since the current block is not the only entry point
            }
            if (not target_bbi.bblock->phis.empty()) {
                // Phi elimination is handled else where, and a phi prevents this optimization
                break;
            }
            merged_bblocks.emplace(target_name);
            bblock.instructions.pop_back(); // remove goto
            for (auto& instr : target_bbi.bblock->instructions) {
                bblock.instructions.emplace_back(std::move(instr));
            }
            // Fix phis
            for (auto const& succ_name : target_bbi.successors) {
                for (auto& [phi_var, preds] : bbinfos.at(succ_name).bblock->phis) {
                    auto nh = preds.extract(target_name);
                    assert(not nh.empty());
                    nh.key() = bblock.name;
                    auto irt = preds.insert(std::move(nh));
                    assert(irt.inserted);
                }
            }
        }
    }
    // Remove remains of the merged blocks
    std::vector<BasicBlock> res;
    for (auto& bblock : body) {
        if (not merged_bblocks.count(bblock.name)) {
            res.emplace_back(std::move(bblock));
        }
    }
    return res;
}

std::vector<BasicBlock> eliminate_dead_code(std::vector<BasicBlock>&& body) {
    body = eliminate_dead_blocks(std::move(body));
    eliminate_dead_variables(body);
    body = eliminate_redundant_gotos(std::move(body));
    return std::move(body);
}

} // namespace ir
