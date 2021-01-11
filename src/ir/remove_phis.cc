#include "src/ir/remove_phis.hh"
#include "src/ir/ir.hh"
#include "src/ir/name_allocator.hh"
#include "src/ir/var_allocator.hh"
#include "src/overloaded.hh"

#include <cassert>

namespace ir {

static std::pair<std::optional<Label>, std::optional<VarName>>
largest_names_in_use(const FnDef& fdef) {
    std::optional<Label> largest_label;
    std::optional<VarName> largest_var_name;
    auto maxi_var = [&largest_var_name](Var var) {
        if (var.name > largest_var_name) {
            largest_var_name = var.name;
        }
    };
    for (auto const& arg : fdef.args) {
        maxi_var(arg);
    }
    for (auto const& bblock : fdef.body) {
        if (bblock.name > largest_label) {
            largest_label = bblock.name;
        }
        for (auto const& [phi_var, _] : bblock.phis) {
            maxi_var(phi_var);
        }
        for (auto const& instr : bblock.instructions) {
            std::visit(
                overloaded{
                    [&](const ICopy& i) { maxi_var(i.var); },
                    [&](const IUnaryOp& i) { maxi_var(i.var); },
                    [&](const IBinOp& i) { maxi_var(i.var); },
                    [&](const ILoad& i) { maxi_var(i.var); },
                    [&](const IConstLoad& i) { maxi_var(i.var); },
                    [&](const IStore& /*unused*/) {
                        // No var definition
                    },
                    [&](const ICall& i) { maxi_var(i.var); },
                    [&](const IVCall& /*unused*/) {
                        // No var definition
                    },
                    [&](const IGoto& /*unused*/) {
                        // No var definition
                    },
                    [&](const IIfUnaryCond& /*unused*/) {
                        // No var definition
                    },
                    [&](const IIfBinCond& /*unused*/) {
                        // No var definition
                    },
                    [&](const IReturn& /*unused*/) {
                        // No var definition
                    },
                    [&](const IUnreachable& /*unused*/) {
                        // No var definition
                    },
                },
                instr);
        }
    }
    return {largest_label, largest_var_name};
}

FnDef remove_phis(FnDef&& fdef) {
    auto [largest_label, largest_var_name] = largest_names_in_use(fdef);
    auto label_allocator =
        largest_label ? NameAllocator<Label>{*largest_label} : NameAllocator<Label>{};
    auto var_allocator = largest_var_name ? VarAllocator{*largest_var_name} : VarAllocator{};

    std::vector<BasicBlock> new_body;
    new_body.reserve(fdef.body.size() * 2);

    std::map<Label, const BasicBlock&> old_bblocks;
    for (auto& bblock : fdef.body) {
        old_bblocks.try_emplace(bblock.name, bblock);
    }

    for (auto& bblock : fdef.body) {
        assert(!bblock.instructions.empty());
        auto& nbb = new_body.emplace_back(BasicBlock{
            .name = bblock.name,
            .phis = {},
            .instructions = std::move(bblock.instructions),
        });
        auto gen_unphi_instructions = [&var_allocator,
                                       &old_bblocks](Label source, Label target) {
            std::vector<Instruction> loads;
            std::vector<Instruction> stores;
            for (auto& [target_phi_var, target_phi_preds] : old_bblocks.at(target).phis) {
                auto source_val = target_phi_preds.at(source);
                if (auto* var = std::get_if<Var>(&source_val); var and target_phi_var == *var)
                {
                    continue; // skip x := x since it is no-op
                }
                auto tmp_var = var_allocator.alloc(target_phi_var.type);
                loads.emplace_back(ICopy{
                    .var = tmp_var,
                    .val = source_val,
                });
                stores.emplace_back(ICopy{
                    .var = target_phi_var,
                    .val = tmp_var,
                });
            }
            loads.reserve(loads.size() + stores.size());
            for (auto& instr : stores) {
                loads.emplace_back(std::move(instr));
            }
            loads.emplace_back(IGoto{.target = target});
            return loads;
        };
        auto gen_unphi_block = [&label_allocator, &new_body,
                                &gen_unphi_instructions](Label source, Label& target) {
            auto new_block_instrs = gen_unphi_instructions(source, target);
            if (new_block_instrs.size() > 1) {
                auto label = label_allocator.alloc();
                new_body.emplace_back(BasicBlock{
                    .name = label,
                    .phis = {},
                    .instructions = std::move(new_block_instrs),
                });
                target = label;
            }
        };
        std::visit(
            overloaded{
                [&](ICopy& /*unused*/) { std::abort(); },
                [&](IUnaryOp& /*unused*/) { std::abort(); },
                [&](IBinOp& /*unused*/) { std::abort(); },
                [&](ILoad& /*unused*/) { std::abort(); },
                [&](IConstLoad& /*unused*/) { std::abort(); },
                [&](IStore& /*unused*/) { std::abort(); },
                [&](ICall& /*unused*/) { std::abort(); },
                [&](IVCall& /*unused*/) { std::abort(); },
                [&](IGoto& i) {
                    auto unphi_instrs = gen_unphi_instructions(nbb.name, i.target);
                    nbb.instructions.pop_back();
                    nbb.instructions.insert(
                        nbb.instructions.end(), unphi_instrs.begin(), unphi_instrs.end());
                },
                [&](IIfUnaryCond& i) {
                    gen_unphi_block(nbb.name, i.true_branch);
                    gen_unphi_block(nbb.name, i.false_branch);
                },
                [&](IIfBinCond& i) {
                    gen_unphi_block(nbb.name, i.true_branch);
                    gen_unphi_block(nbb.name, i.false_branch);
                },
                [&](IReturn& /*unused*/) {
                    // Nothing to do
                },
                [&](IUnreachable& /*unused*/) {
                    // Nothing to do
                },

            },
            nbb.instructions.back());
    }
    return {
        .name = std::move(fdef.name),
        .ret_type = fdef.ret_type,
        .args = std::move(fdef.args),
        .body = std::move(new_body),
    };
}

} // namespace ir
