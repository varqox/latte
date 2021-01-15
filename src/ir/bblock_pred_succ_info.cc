#include "src/ir/bblock_pred_succ_info.hh"
#include "src/ir/ir.hh"
#include "src/overloaded.hh"

#include <cassert>
#include <map>
#include <vector>

namespace ir {

std::map<Label, BBInfo> bblocks_pred_succ_info(std::vector<BasicBlock>& body) {
    std::map<Label, BBInfo> bbinfos;
    for (auto& bblock : body) {
        std::vector<Label> successors;
        assert(!bblock.instructions.empty());
        std::visit(
            overloaded{
                [&](const ICopy& /*unused*/) { std::abort(); },
                [&](const IUnaryOp& /*unused*/) { std::abort(); },
                [&](const IBinOp& /*unused*/) { std::abort(); },
                [&](const ILoad& /*unused*/) { std::abort(); },
                [&](const IConstLoad& /*unused*/) { std::abort(); },
                [&](const IStore& /*unused*/) { std::abort(); },
                [&](const ICall& /*unused*/) { std::abort(); },
                [&](const IVCall& /*unused*/) { std::abort(); },
                [&](const IGoto& i) { successors.emplace_back(i.target); },
                [&](const IIfUnaryCond& i) {
                    successors.emplace_back(i.true_branch);
                    if (i.false_branch != i.true_branch) {
                        successors.emplace_back(i.false_branch);
                    }
                },
                [&](const IIfBinCond& i) {
                    successors.emplace_back(i.true_branch);
                    if (i.false_branch != i.true_branch) {
                        successors.emplace_back(i.false_branch);
                    }
                },
                [&](const IReturn& /*unused*/) {
                    // Nothing to do
                },
                [&](const IUnreachable& /*unused*/) {
                    // Nothing to do
                },
            },
            bblock.instructions.back());

        bbinfos.emplace(
            bblock.name,
            BBInfo{
                .bblock = &bblock,
                .predecessors = {},
                .successors = std::move(successors),
            });
    }
    // Fill predecessors
    for (auto const& [name, bbi] : bbinfos) {
        for (auto pred : bbi.successors) {
            bbinfos.at(pred).predecessors.emplace(name);
        }
    }
    return bbinfos;
}

} // namespace ir
