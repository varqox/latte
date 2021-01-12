#include "src/ir/propagate_constants.hh"
#include "src/define_cmp_operators_by_field.hh"
#include "src/defs.hh"
#include "src/ir/ir.hh"
#include "src/overloaded.hh"

#include <cassert>
#include <map>
#include <set>
#include <type_traits>
#include <variant>
#include <vector>

namespace {

template <class T>
T wrapped_add(T a, T b) {
    T c = 0;
    __builtin_add_overflow(a, b, &c);
    return c;
}
template <class T>
T wrapped_sub(T a, T b) {
    T c = 0;
    __builtin_sub_overflow(a, b, &c);
    return c;
}
template <class T>
T wrapped_mul(T a, T b) {
    T c = 0;
    __builtin_mul_overflow(a, b, &c);
    return c;
}
template <class T>
T wrapped_div(T a, T b) {
    T c = 0;
    if (b != -1 and b != 0) {
        c = a / b;
    }
    return c;
}
template <class T>
T wrapped_mod(T a, T b) {
    T c = 0;
    if (b != -1 and b != 0) {
        c = a % b;
    }
    return c;
}

class ConstantPropagator {
    struct InstrInfo {
        ir::Instruction* instr;
        ir::Label bblock_name;

        bool operator<(const InstrInfo& b) const noexcept { return instr < b.instr; }
    };

    std::map<ir::Var, ir::Value> constants;
    std::set<InstrInfo> for_reduction;
    std::map<ir::Var, std::vector<InstrInfo>> instrs_using_var;
    std::map<ir::Label, ir::BasicBlock*> bblocks;

    [[nodiscard]] static bool is_constant(const ir::Value& val) noexcept {
        return std::visit(
            overloaded{
                [](const ir::Var& /*unused*/) { return false; },
                [](int_t /*unused*/) { return true; },
                [](bool /*unused*/) { return true; },
                [](const ir::Null& /*unused*/) { return true; },
                [](const ir::StringConstantName& /*unused*/) { return true; },
                [](const ir::VTableName& /*unused*/) { return true; },
            },
            val);
    }

    void prepare(ir::Label bblock_name, ir::Instruction& instr) {
        auto add_instr_to_var = [&](ir::Var var) {
            auto& vec = instrs_using_var[var];
            if (vec.empty() or vec.back().instr != &instr) {
                vec.emplace_back(InstrInfo{
                    .instr = &instr,
                    .bblock_name = bblock_name,
                });
            }
        };
        auto try_add_constant = [&](ir::Var var, const ir::Value& val) {
            if (is_constant(val)) {
                auto [_, inserted] = constants.insert_or_assign(var, val);
                assert(inserted);
            }
        };
        auto prepare_mloc = [&](ir::MemLoc& mloc) {
            std::visit(
                overloaded{
                    [&](ir::Var v) { add_instr_to_var(v); },
                    [&](ir::Null /*unused*/) {},
                },
                mloc.base);
            if (mloc.scale != 0) {
                std::visit(
                    overloaded{
                        [&](ir::Var v) { add_instr_to_var(v); },
                        [&](int_t& i) {
                            mloc.displacement = wrapped_add(
                                mloc.displacement, wrapped_mul<size_t>(mloc.scale, i));
                            i = 0;
                            mloc.scale = 0;
                        }},
                    mloc.index);
            }
        };
        auto prepare_cmloc = [&](ir::ConstMemLoc& cmloc) { prepare_mloc(cmloc.loc); };
        auto prepare_val = [&](const ir::Value& val) {
            std::visit(
                overloaded{
                    [&](ir::Var v) { add_instr_to_var(v); },
                    [&](int_t /*unused*/) {},
                    [&](bool /*unused*/) {},
                    [&](const ir::Null& /*unused*/) {},
                    [&](const ir::StringConstantName& /*unused*/) {},
                    [&](const ir::VTableName& /*unused*/) {},
                },
                val);
        };
        auto prepare_func = [&](std::variant<ir::FnName, ir::ConstMemLoc>& func) {
            std::visit(
                overloaded{
                    [&](const ir::FnName& /*unused*/) {},
                    [&](ir::ConstMemLoc& cmloc) { prepare_cmloc(cmloc); }},
                func);
        };
        std::visit(
            overloaded{
                [&](ir::ICopy& i) {
                    add_instr_to_var(i.var);
                    prepare_val(i.val);
                    try_add_constant(i.var, i.val);
                },
                [&](ir::IUnaryOp& i) {
                    add_instr_to_var(i.var);
                    prepare_val(i.val);
                    if (is_constant(i.val)) {
                        for_reduction.emplace(
                            InstrInfo{.instr = &instr, .bblock_name = bblock_name});
                    }
                },
                [&](ir::IBinOp& i) {
                    add_instr_to_var(i.var);
                    prepare_val(i.left);
                    prepare_val(i.right);
                    if (is_constant(i.left) and is_constant(i.right)) {
                        for_reduction.emplace(
                            InstrInfo{.instr = &instr, .bblock_name = bblock_name});
                    }
                },
                [&](ir::ILoad& i) {
                    add_instr_to_var(i.var);
                    prepare_mloc(i.loc);
                },
                [&](ir::IConstLoad& i) {
                    add_instr_to_var(i.var);
                    prepare_cmloc(i.loc);
                },
                [&](ir::IStore& i) {
                    prepare_mloc(i.loc);
                    prepare_val(i.val);
                },
                [&](ir::ICall& i) {
                    add_instr_to_var(i.var);
                    prepare_func(i.func);
                    for (auto& arg : i.args) {
                        prepare_val(arg);
                    }
                },
                [&](ir::IVCall& i) {
                    prepare_func(i.func);
                    for (auto& arg : i.args) {
                        prepare_val(arg);
                    }
                },
                [&](ir::IGoto& /*unused*/) {
                    // Nothing to do
                },
                [&](ir::IIfUnaryCond& i) {
                    prepare_val(i.cond);
                    if (is_constant(i.cond) or i.true_branch == i.false_branch) {
                        for_reduction.emplace(
                            InstrInfo{.instr = &instr, .bblock_name = bblock_name});
                    }
                },
                [&](ir::IIfBinCond& i) {
                    prepare_val(i.left);
                    prepare_val(i.right);
                    if ((is_constant(i.left) and is_constant(i.right)) or
                        i.true_branch == i.false_branch) {
                        for_reduction.emplace(
                            InstrInfo{.instr = &instr, .bblock_name = bblock_name});
                    }
                },
                [&](ir::IReturn& i) {
                    if (i.val) {
                        prepare_val(*i.val);
                    }
                },
                [&](ir::IUnreachable& /*unused*/) {
                    // Nothing to do
                },
            },
            instr);
    }

    std::optional<ir::Value> constant_var_value(ir::Var var) {
        auto it = constants.find(var);
        if (it == constants.end()) {
            return std::nullopt;
        }
        return it->second;
    }

    ir::Value reduce(ir::Value&& val) {
        return std::visit(
            overloaded{
                [&](ir::Var v) -> ir::Value { return constant_var_value(v).value_or(v); },
                [&](int_t i) -> ir::Value { return i; },
                [&](bool b) -> ir::Value { return b; },
                [&](ir::Null&& n) -> ir::Value { return n; },
                [&](ir::StringConstantName&& scn) -> ir::Value { return scn; },
                [&](ir::VTableName&& vtn) -> ir::Value { return std::move(vtn); },
            },
            std::move(val));
    }

    ir::MemLoc reduce(ir::MemLoc&& mloc) {
        mloc.base = std::visit(
            overloaded{
                [&](ir::Var v) -> decltype(mloc.base) {
                    auto vval = constant_var_value(v);
                    if (vval) {
                        if (std::holds_alternative<ir::Null>(*vval)) {
                            return ir::Null{};
                        }
                    }
                    return v;
                },
                [&](ir::Null n) -> decltype(mloc.base) { return n; },
            },
            mloc.base);

        if (mloc.scale == 0) {
            mloc.index = 0;
        } else {
            mloc.index = std::visit(
                overloaded{
                    [&](ir::Var v) -> decltype(mloc.index) {
                        auto vval = constant_var_value(v);
                        if (vval) {
                            mloc.displacement = wrapped_add(
                                mloc.displacement,
                                wrapped_mul<size_t>(mloc.scale, std::get<int_t>(*vval)));
                            mloc.scale = 0;
                            return 0;
                        }
                        return v;
                    },
                    [&](int_t i) -> decltype(mloc.index) {
                        mloc.displacement =
                            wrapped_add(mloc.displacement, wrapped_mul<size_t>(mloc.scale, i));
                        mloc.scale = 0;
                        return 0;
                    },
                },
                mloc.index);
        }

        return mloc;
    }

    ir::ConstMemLoc reduce(ir::ConstMemLoc&& cmloc) {
        return {
            .loc = reduce(std::move(cmloc.loc)), // NOLINT(performance-move-const-arg)
        };
    }

    decltype(ir::ICall::func) reduce(decltype(ir::ICall::func)&& func) {
        return std::visit(
            overloaded{
                [&](ir::FnName&& fname) -> decltype(ir::ICall::func) {
                    return std::move(fname);
                },
                [&](ir::ConstMemLoc&& cmloc) -> decltype(ir::ICall::func) {
                    return reduce(std::move(cmloc)); // NOLINT(performance-move-const-arg)
                },
            },
            std::move(func));
    }

    ir::Instruction reduce(ir::Label bblock_name, ir::Instruction&& instr) {
        auto new_constant_var = [&](ir::Var var, const ir::Value& val) {
            auto [_, inserted] = constants.try_emplace(var, val);
            if (inserted) {
                for (auto iinfo : instrs_using_var.at(var)) {
                    for_reduction.emplace(iinfo);
                }
            }
        };
        auto reduce_if_instr = [&](bool cond_val, ir::Label true_label,
                                   ir::Label false_label) {
            auto alive = (cond_val ? true_label : false_label);
            auto dead = (cond_val ? false_label : true_label);
            for (auto& [var, preds] : bblocks.at(dead)->phis) {
                preds.erase(bblock_name);
            }
            return ir::IGoto{
                .target = alive,
            };
        };
        return std::visit(
            overloaded{
                [&](ir::ICopy&& i) -> ir::Instruction {
                    auto val = reduce(std::move(i.val));
                    if (is_constant(val)) {
                        new_constant_var(i.var, val);
                    }
                    return ir::ICopy{
                        .var = i.var,
                        .val = std::move(val),
                    };
                },
                [&](ir::IUnaryOp&& i) -> ir::Instruction {
                    auto val = reduce(std::move(i.val));
                    if (not is_constant(val)) {
                        return ir::IUnaryOp{
                            .var = i.var,
                            .op = i.op,
                            .val = std::move(val),
                        };
                    }
                    switch (i.op) {
                    case ir::UnaryOp::NEG: {
                        return ir::ICopy{
                            .var = i.var,
                            .val = wrapped_sub(0, std::get<int_t>(val)),
                        };
                    }
                    case ir::UnaryOp::NOT: {
                        return ir::ICopy{
                            .var = i.var,
                            .val = !std::get<bool>(val),
                        };
                    }
                    }
                    __builtin_unreachable();
                },
                [&](ir::IBinOp&& i) -> ir::Instruction {
                    auto left = reduce(std::move(i.left));
                    auto right = reduce(std::move(i.right));
                    if (not is_constant(left) or not is_constant(right)) {
                        return ir::IBinOp{
                            .var = i.var,
                            .op = i.op,
                            .left = std::move(left),
                            .right = std::move(right),
                        };
                    }
                    auto lval = std::get<int_t>(left);
                    auto rval = std::get<int_t>(right);
                    int_t res{};
                    switch (i.op) {
                    case ir::BinOp::ADD: {
                        res = wrapped_add(lval, rval);
                    } break;
                    case ir::BinOp::SUB: {
                        res = wrapped_sub(lval, rval);
                    } break;
                    case ir::BinOp::MUL: {
                        res = wrapped_mul(lval, rval);
                    } break;
                    case ir::BinOp::DIV: {
                        res = wrapped_div(lval, rval);
                    } break;
                    case ir::BinOp::MOD: {
                        res = wrapped_mod(lval, rval);
                    } break;
                    }
                    return ir::ICopy{
                        .var = i.var,
                        .val = res,
                    };
                },
                [&](ir::ILoad&& i) -> ir::Instruction {
                    i.loc = reduce(std::move(i.loc)); // NOLINT(performance-move-const-arg)
                    return i;
                },
                [&](ir::IConstLoad&& i) -> ir::Instruction {
                    i.loc = reduce(std::move(i.loc)); // NOLINT(performance-move-const-arg)
                    return i;
                },
                [&](ir::IStore&& i) -> ir::Instruction {
                    return ir::IStore{
                        .loc = reduce(std::move(i.loc)), // NOLINT(performance-move-const-arg)
                        .val = reduce(std::move(i.val)),
                    };
                },
                [&](ir::ICall&& i) -> ir::Instruction {
                    i.func = reduce(std::move(i.func));
                    for (auto& arg : i.args) {
                        arg = reduce(std::move(arg));
                    }
                    return std::move(i);
                },
                [&](ir::IVCall&& i) -> ir::Instruction {
                    i.func = reduce(std::move(i.func));
                    for (auto& arg : i.args) {
                        arg = reduce(std::move(arg));
                    }
                    return std::move(i);
                },
                [&](ir::IGoto&& i) -> ir::Instruction {
                    return i; // Nothing to do
                },
                [&](ir::IIfUnaryCond&& i) -> ir::Instruction {
                    if (i.true_branch == i.false_branch) {
                        return ir::IGoto{
                            .target = i.true_branch,
                        };
                    }
                    auto cond = constant_var_value(i.cond);
                    if (!cond) {
                        return i;
                    }
                    return reduce_if_instr(
                        std::get<bool>(*cond) ^ i.negate_cond, i.true_branch, i.false_branch);
                },
                [&](ir::IIfBinCond&& i) -> ir::Instruction {
                    if (i.true_branch == i.false_branch) {
                        return ir::IGoto{
                            .target = i.true_branch,
                        };
                    }
                    auto left = reduce(std::move(i.left));
                    auto right = reduce(std::move(i.right));
                    if (!((is_constant(left) and is_constant(right)) or left == right)) {
                        return ir::IIfBinCond{
                            .op = i.op,
                            .left = std::move(left),
                            .right = std::move(right),
                            .true_branch = i.true_branch,
                            .false_branch = i.false_branch,
                        };
                    }
                    auto cond_val = [&] {
                        switch (i.op) {
                        case ir::RelOp::LTH: return left < right;
                        case ir::RelOp::LE: return left <= right;
                        case ir::RelOp::GTH: return left > right;
                        case ir::RelOp::GE: return left >= right;
                        case ir::RelOp::EQ: return left == right;
                        case ir::RelOp::NE: return left != right;
                        }
                        __builtin_unreachable();
                    }();
                    return reduce_if_instr(cond_val, i.true_branch, i.false_branch);
                },
                [&](ir::IReturn&& i) -> ir::Instruction {
                    if (i.val) {
                        auto val = reduce(std::move(*i.val));
                        return ir::IReturn{
                            .val = std::move(val),
                        };
                    }
                    return std::move(i);
                },
                [&](ir::IUnreachable&& i) -> ir::Instruction {
                    return i; // Nothing to do
                },
            },
            std::move(instr));
    }

public:
    ConstantPropagator() = default;

    void propagate_constants(std::vector<ir::BasicBlock>& body) && {
        for (auto& bblock : body) {
            for (auto& instr : bblock.instructions) {
                prepare(bblock.name, instr);
            }
        }
        for (auto& bblock : body) {
            bblocks.emplace(bblock.name, &bblock);
        }
        for (auto& [var, _] : constants) {
            for (auto iinfo : instrs_using_var.at(var)) {
                for_reduction.emplace(iinfo);
            }
        }
        // Reduce instructions
        while (not for_reduction.empty()) {
            auto iinfo = *for_reduction.begin();
            for_reduction.erase(for_reduction.begin());
            *iinfo.instr = reduce(iinfo.bblock_name, std::move(*iinfo.instr));
        }
        // Reduce phis
        for (auto& bblock : body) {
            for (auto&& [var, preds] : bblock.phis) {
                for (auto&& [label, val] : preds) {
                    val = reduce(std::move(val));
                }
            }
        }
    }
};

} // namespace

namespace ir {

[[nodiscard]] std::vector<BasicBlock> propagate_constants(std::vector<BasicBlock>&& body) {
    ConstantPropagator{}.propagate_constants(body);
    return std::move(body);
}

} // namespace ir
