#include "src/ir/global_subexpression_elimination.hh"
#include "src/ir/bblock_pred_succ_info.hh"
#include "src/ir/ir.hh"
#include "src/overloaded.hh"
#include "src/ranges.hh"

#include <cassert>
#include <map>
#include <optional>
#include <set>
#include <utility>
#include <vector>

namespace {

struct DomRel {
    std::set<std::pair<ir::Label, ir::Label>> edges; // contains (a, b) iff a dominates b
    std::vector<ir::Label> topo_order; // root is first
};

[[nodiscard]] std::pair<DomRel, std::map<ir::Label, ir::BBInfo>>
compute_dominators(std::vector<ir::BasicBlock>& bblocks) {
    auto bbinfos = bblocks_pred_succ_info(bblocks);
    DomRel dom_rel;
    std::set<ir::Label> visited;
    std::vector<ir::Label> stack;
    for (auto const& bblock : bblocks) {
        visited = {bblock.name};
        stack.emplace_back(bblocks.front().name);
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
        for (auto const& other_bblock : bblocks) {
            if (not visited.count(other_bblock.name)) {
                dom_rel.edges.emplace(bblock.name, other_bblock.name);
            }
        }
    }
    // Toposort
    std::map<ir::Label, size_t> in_deg;
    for (auto const& [_, b] : dom_rel.edges) {
        ++in_deg[b];
    }
    assert(stack.empty());
    for (auto const& bblock : reverse_view(bblocks)) { // top stack elem will be the root
        if (in_deg[bblock.name] == 0) {
            stack.emplace_back(bblock.name);
        }
    }
    while (not stack.empty()) {
        auto x = stack.back();
        stack.pop_back();
        dom_rel.topo_order.emplace_back(x);
        // Find first edge
        auto it = dom_rel.edges.lower_bound({x, x});
        while (it != dom_rel.edges.begin()) {
            auto prev_it = std::prev(it);
            if (prev_it->first < x) {
                break;
            }
            it = prev_it;
        }
        // Process edges
        while (it != dom_rel.edges.end() and it->first == x) {
            if (--in_deg.at(it->second) == 0) {
                stack.emplace_back(it->second);
            }
            ++it;
        }
    }
    assert(dom_rel.topo_order.size() == bblocks.size());
    return {std::move(dom_rel), std::move(bbinfos)};
}

struct ValUnaryOp {
    ir::UnaryOp op;
    ir::Value val;

    friend bool operator<(const ValUnaryOp& a, const ValUnaryOp& b) {
        return std::tuple{a.op, a.val} < std::tuple{b.op, b.val};
    }
};

struct ValBinOp {
    ir::BinOp op;
    ir::Value left;
    ir::Value right; // TODO: remember to construct in such a way that left <= right

    friend bool operator<(const ValBinOp& a, const ValBinOp& b) {
        return std::tuple{a.op, a.left, a.right} < std::tuple{b.op, b.left, b.right};
    }
};

using GCSEValue = std::variant<ir::Value, ir::ConstMemLoc, ValUnaryOp, ValBinOp>;

} // namespace

namespace ir {

std::vector<BasicBlock> global_subexpression_elimination(std::vector<BasicBlock>&& body) {
    auto [dom_rel, bbinfos] = compute_dominators(body);
    std::map<GCSEValue, std::map<Label, ir::Var>> gcse_exprs;
    auto dominates = [&, &dom_rel = dom_rel](Label a, Label b) {
        return dom_rel.edges.count({a, b}) == 1;
    };
    auto reduced = [&](Label bblock_name, const GCSEValue& val) -> std::optional<Var> {
        auto it = gcse_exprs.find(val);
        if (it == gcse_exprs.end()) {
            return std::nullopt;
        }
        std::optional<std::pair<Label, ir::Var>> most_dominating;
        for (auto const& [bbn, var] : it->second) {
            if (bbn == bblock_name or dominates(bbn, bblock_name)) {
                if (not most_dominating or dominates(bbn, most_dominating->first)) {
                    most_dominating = {bbn, var};
                }
            }
        }
        if (not most_dominating) {
            return std::nullopt;
        }
        return most_dominating->second;
    };
    auto reduce_func = [&](Label bblock_name, decltype(ICall::func)& func) {
        func = std::visit(
            overloaded{
                [&](FnName&& fname) -> decltype(ICall::func) { return std::move(fname); },
                [&](ConstMemLoc&& cmloc) -> decltype(ICall::func) {
                    if (auto matched_var = reduced(bblock_name, cmloc)) {
                        return *matched_var;
                    }
                    return cmloc;
                },
                [&](Value&& val) -> decltype(ICall::func) {
                    if (auto matched_var = reduced(bblock_name, val)) {
                        return *matched_var;
                    }
                    return std::move(val);
                },
            },
            std::move(func));
    };
    auto reduce_args = [&](Label bblock_name, std::vector<Value>& args) {
        for (auto& arg : args) {
            if (auto matched_var = reduced(bblock_name, arg)) {
                arg = *matched_var;
            }
        }
    };
    auto process_instr = [&](Label bblock_name, Instruction& instr) {
        instr = std::visit(
            overloaded{
                [&](ICopy&& i) -> Instruction {
                    auto gcse_val = i.val;
                    if (auto matched_var = reduced(bblock_name, gcse_val)) {
                        return ICopy{
                            .var = i.var,
                            .val = *matched_var,
                        };
                    }
                    gcse_exprs[gcse_val][bblock_name] = i.var;
                    return std::move(i);
                },
                [&](IUnaryOp&& i) -> Instruction {
                    auto gcse_val = ValUnaryOp{
                        .op = i.op,
                        .val = i.val,
                    };
                    auto matched_var = reduced(bblock_name, gcse_val);
                    if (matched_var) {
                        return ICopy{
                            .var = i.var,
                            .val = *matched_var,
                        };
                    }
                    gcse_exprs[gcse_val][bblock_name] = i.var;
                    return std::move(i);
                },
                [&](IBinOp&& i) -> Instruction {
                    auto gcse_val = [&] {
                        auto left = i.left;
                        auto right = i.right;
                        if (left > right) {
                            switch (i.op) {
                            case BinOp::ADD:
                            case BinOp::MUL: swap(left, right); break;
                            case BinOp::SUB:
                            case BinOp::DIV:
                            case BinOp::MOD: break;
                            }
                        }
                        return ValBinOp{
                            .op = i.op,
                            .left = std::move(left),
                            .right = std::move(right),
                        };
                    }();
                    if (auto matched_var = reduced(bblock_name, gcse_val)) {
                        return ICopy{
                            .var = i.var,
                            .val = *matched_var,
                        };
                    }
                    gcse_exprs[gcse_val][bblock_name] = i.var;
                    return std::move(i);
                },
                [&](ILoad&& i) -> Instruction {
                    return i; // non-const load cannot be optimized
                },
                [&](IConstLoad&& i) -> Instruction {
                    auto gcse_val = i.loc;
                    if (auto matched_var = reduced(bblock_name, gcse_val)) {
                        return ICopy{
                            .var = i.var,
                            .val = *matched_var,
                        };
                    }
                    gcse_exprs[gcse_val][bblock_name] = i.var;
                    return i;
                },
                [&](IStore&& i) -> Instruction {
                    auto matched_var = reduced(bblock_name, i.val);
                    if (matched_var) {
                        i.val = *matched_var;
                    }
                    return std::move(i);
                },
                [&](ICall&& i) -> Instruction {
                    reduce_func(bblock_name, i.func);
                    reduce_args(bblock_name, i.args);
                    return std::move(i);
                },
                [&](IVCall&& i) -> Instruction {
                    reduce_func(bblock_name, i.func);
                    reduce_args(bblock_name, i.args);
                    return std::move(i);
                },
                [&](IGoto&& i) -> Instruction { return i; },
                [&](IIfUnaryCond&& i) -> Instruction { return i; },
                [&](IIfBinCond&& i) -> Instruction { return std::move(i); },
                [&](IReturn&& i) -> Instruction {
                    if (i.val) {
                        if (auto matched_var = reduced(bblock_name, *i.val)) {
                            i.val = *matched_var;
                        }
                    }
                    return std::move(i);
                },
                [&](IUnreachable&& i) -> Instruction { return i; },
            },
            std::move(instr));
    };
    for (auto bbname : dom_rel.topo_order) {
        auto& bblock = *bbinfos.at(bbname).bblock;
        for (auto& instr : bblock.instructions) {
            process_instr(bbname, instr);
        }
    }
    return std::move(body);
}

} // namespace ir
