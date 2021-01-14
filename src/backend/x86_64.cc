#include "src/backend/x86_64.hh"
#include "src/concat.hh"
#include "src/ends_with.hh"
#include "src/ir/ir.hh"
#include "src/ir/ir_printer.hh"
#include "src/ir/remove_phis.hh"
#include "src/overloaded.hh"
#include "src/ranges.hh"

#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>

namespace {

enum Reg {
    RAX,
    RBX,
    RCX,
    RDX,
    RDI,
    RSI,
    RSP,
    RBP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
};

constexpr const char* to_asm(Reg reg, ir::Type type) noexcept {
    switch (type) {
    case ir::Type::BOOL: {
        switch (reg) {
        case RAX: return "al";
        case RBX: return "bl";
        case RCX: return "cl";
        case RDX: return "dl";
        case RDI: return "dil";
        case RSI: return "sil";
        case RSP: return "spl";
        case RBP: return "bpl";
        case R8: return "r8b";
        case R9: return "r9b";
        case R10: return "r10b";
        case R11: return "r11b";
        case R12: return "r12b";
        case R13: return "r13b";
        case R14: return "r14b";
        case R15: return "r15b";
        }
        __builtin_unreachable();
    }
    case ir::Type::INT: {
        switch (reg) {
        case RAX: return "eax";
        case RBX: return "ebx";
        case RCX: return "ecx";
        case RDX: return "edx";
        case RDI: return "edi";
        case RSI: return "esi";
        case RSP: return "esp";
        case RBP: return "ebp";
        case R8: return "r8d";
        case R9: return "r9d";
        case R10: return "r10d";
        case R11: return "r11d";
        case R12: return "r12d";
        case R13: return "r13d";
        case R14: return "r14d";
        case R15: return "r15d";
        }
        __builtin_unreachable();
    }
    case ir::Type::PTR: {
        switch (reg) {
        case RAX: return "rax";
        case RBX: return "rbx";
        case RCX: return "rcx";
        case RDX: return "rdx";
        case RDI: return "rdi";
        case RSI: return "rsi";
        case RSP: return "rsp";
        case RBP: return "rbp";
        case R8: return "r8";
        case R9: return "r9";
        case R10: return "r10";
        case R11: return "r11";
        case R12: return "r12";
        case R13: return "r13";
        case R14: return "r14";
        case R15: return "r15";
        }
        __builtin_unreachable();
    }
    }
    __builtin_unreachable();
}

class SimpleFnEmitter {
    size_t stack_size_needed = 0;
    const ir::FnDef* fdef = nullptr;
    std::string code;
    std::set<Reg> regs_used;
    std::set<Reg> regs_available = {RAX, RBX, RCX, RDX, RDI, RSI, R8,
                                    R9,  R10, R11, R12, R13, R14, R15};
    std::optional<ir::Label> next_bblock_name;
    static constexpr auto epilogue_label = ".epilogue";

    static constexpr const char* loc_code_prefix(ir::Type type) noexcept {
        switch (type) {
        case ir::Type::BOOL: return "byte ";
        case ir::Type::INT: return "dword ";
        case ir::Type::PTR: return "qword ";
        }
        __builtin_unreachable();
    }

    struct [[nodiscard]] VarLocCode {
        std::string code;
    };

    VarLocCode var_loc_code(ir::Var var) {
        if (var.name.id < fdef->args.size()) {
            // arg
            return {
                .code =
                    concat(loc_code_prefix(var.type), "[rbp + ", 16 + 8 * var.name.id, ']'),
            };
        }
        // local variable
        auto stack_var_id = 1 + var.name.id - fdef->args.size(); // numbered from 1
        stack_size_needed = std::max(stack_size_needed, stack_var_id * 8);
        return {
            .code = concat(loc_code_prefix(var.type), "[rbp - ", 8 * stack_var_id, ']'),
        };
    }

    struct RegVal {
        Reg reg;
        ir::Type type;
    };

    struct [[nodiscard]] RegHolder {
        std::optional<Reg> reg;
        ir::Type type;
        SimpleFnEmitter* emitter;

        RegHolder(Reg reg, ir::Type type, SimpleFnEmitter* emitter) noexcept
        : reg{reg}
        , type{type}
        , emitter{emitter} {}

        RegHolder(const RegHolder&) = delete;
        RegHolder& operator=(const RegHolder&) = delete;

        RegHolder(RegHolder&& other) noexcept
        : reg{std::exchange(other.reg, std::nullopt)}
        , type{other.type}
        , emitter{other.emitter} {}

        RegHolder& operator=(RegHolder&& other) noexcept {
            if (reg) {
                emitter->release_reg(*reg);
            }
            reg = std::exchange(other.reg, std::nullopt);
            type = other.type;
            emitter = other.emitter;
            return *this;
        }

        // NOLINTNEXTLINE(google-explicit-constructor)
        operator RegVal() const noexcept {
            assert(reg.has_value());
            return {
                .reg = reg.value(),
                .type = type,
            };
        }

        ~RegHolder() {
            if (reg) {
                emitter->release_reg(*reg);
            }
        }
    };

    RegHolder some_reg(ir::Type type) {
        assert(!regs_available.empty());
        auto reg = *regs_available.begin();
        regs_available.erase(regs_available.begin());
        regs_used.emplace(reg);
        return {reg, type, this};
    }

    RegHolder specific_reg(ir::Type type, Reg reg) {
        assert(regs_available.count(reg));
        regs_available.erase(reg);
        regs_used.emplace(reg);
        return {reg, type, this};
    }

    void release_reg(Reg reg) {
        auto inserted = regs_available.emplace(reg).second;
        assert(inserted); // releasing already released register
    }

    struct AsmiInstrWrapper;

    struct [[nodiscard]] MemLocCode {
        std::vector<std::unique_ptr<AsmiInstrWrapper>> preparing_instructions;
        std::vector<RegHolder> taken_regs;
        std::string code;
    };

    struct [[nodiscard]] asmi {
        struct mov {
            std::variant<
                std::pair<RegVal, VarLocCode>, std::pair<VarLocCode, RegVal>,
                std::pair<RegVal, int_t>, std::pair<RegVal, MemLocCode>,
                std::pair<MemLocCode, RegVal>>
                val;

            explicit mov(RegVal a, VarLocCode b)
            : val{std::pair{a, std::move(b)}} {}
            explicit mov(VarLocCode a, RegVal b)
            : val{std::pair{std::move(a), b}} {}
            explicit mov(RegVal a, int_t b)
            : val{std::pair{a, b}} {}
            explicit mov(RegVal a, MemLocCode b)
            : val{std::pair{a, std::move(b)}} {}
            explicit mov(MemLocCode a, RegVal b)
            : val{std::pair{std::move(a), b}} {}
        };
        struct lea {
            RegVal reg;
            std::variant<ir::StringConstantName, ir::VTableName> val;

            explicit lea(RegVal reg, ir::StringConstantName val)
            : reg{reg}
            , val{val} {}
            explicit lea(RegVal reg, ir::VTableName val)
            : reg{reg}
            , val{std::move(val)} {}
        };
        struct neg {
            RegVal reg;
        };
        struct xor_ {
            RegVal left;
            int_t right;
        };
        struct add {
            RegVal left;
            RegVal right;
        };
        struct sub {
            RegVal left;
            RegVal right;
        };
        struct imul {
            RegVal left;
            RegVal right;
        };
        struct cdq {};
        struct idiv {
            RegVal reg;
        };
        struct test {
            RegVal left;
            RegVal right;
        };
        struct cmp {
            RegVal left;
            RegVal right;
        };
        struct jmp {
            std::variant<ir::Label, std::string> val;
            template <class A>
            explicit jmp(A a)
            : val{std::move(a)} {}
        };
        struct jz {
            ir::Label target;
        };
        struct jnz {
            ir::Label target;
        };
        struct jl {
            ir::Label target;
        };
        struct jle {
            ir::Label target;
        };
        struct jg {
            ir::Label target;
        };
        struct jge {
            ir::Label target;
        };
        struct je {
            ir::Label target;
        };
        struct jne {
            ir::Label target;
        };
        struct push {
            RegVal reg;
        };
        struct add_rsp {
            uint64_t how_much;
        };
        struct call {
            std::variant<std::tuple<ir::FnName>, std::tuple<MemLocCode>> val;
            explicit call(ir::FnName a)
            : val{std::move(a)} {}
            explicit call(MemLocCode a)
            : val{std::move(a)} {}
        };
        using Instr = std::variant<
            mov, lea, neg, xor_, add, sub, imul, cdq, idiv, test, cmp, jmp, jz, jnz, jl, jle,
            jg, jge, je, jne, push, add_rsp, call>;
    };

    struct AsmiInstrWrapper {
        asmi::Instr instr;
    };

    asmi::Instr set_to(const RegHolder& reg, const ir::Value& val) {
        assert(reg.type == type_of(val));
        return std::visit(
            overloaded{
                [&](const ir::Var& var) -> asmi::Instr {
                    return asmi::mov{reg, var_loc_code(var)};
                },
                [&](int_t i) -> asmi::Instr {
                    return asmi::mov{reg, i};
                },
                [&](bool b) -> asmi::Instr {
                    return asmi::mov{reg, b ? 1 : 0};
                },
                [&](ir::Null /*unused*/) -> asmi::Instr {
                    return asmi::mov{reg, 0};
                },
                [&](const ir::StringConstantName& sc) -> asmi::Instr {
                    return asmi::lea{reg, sc};
                },
                [&](const ir::VTableName& vt) -> asmi::Instr {
                    return asmi::lea{reg, vt};
                },
            },
            val);
    }

    MemLocCode mloc_code(const ir::MemLoc& mloc, ir::Type type) {
        MemLocCode res;
        back_insert(res.code, loc_code_prefix(type), '[');
        std::visit(
            overloaded{
                [&](const ir::Var& var) {
                    auto& reg = res.taken_regs.emplace_back(some_reg(ir::Type::PTR));
                    res.preparing_instructions.emplace_back(std::make_unique<AsmiInstrWrapper>(
                        AsmiInstrWrapper{.instr = asmi::mov{reg, var_loc_code(var)}}));
                    back_insert(res.code, to_asm(reg.reg.value(), ir::Type::PTR));
                },
                [&](const ir::Null& /*unused*/) { back_insert(res.code, '0'); },
            },
            mloc.base);
        if (mloc.scale != 0) {
            back_insert(res.code, " + ");
            if (mloc.scale != 1) {
                back_insert(res.code, mloc.scale, " * ");
            }
            std::visit(
                overloaded{
                    [&](const ir::Var& var) {
                        auto& reg = res.taken_regs.emplace_back(some_reg(ir::Type::INT));
                        res.preparing_instructions.emplace_back(
                            std::make_unique<AsmiInstrWrapper>(
                                AsmiInstrWrapper{.instr = asmi::mov{reg, var_loc_code(var)}}));
                        back_insert(res.code, to_asm(reg.reg.value(), ir::Type::PTR));
                    },
                    [&](int_t i) { back_insert(res.code, i); },
                },
                mloc.index);
        }
        if (mloc.displacement != 0) {
            back_insert(res.code, " + ", mloc.displacement);
        }
        back_insert(res.code, ']');
        return res;
    }

    MemLocCode mloc_code(const ir::ConstMemLoc& cmloc, ir::Type type) {
        return mloc_code(cmloc.loc, type);
    }

    void emit_instr(const asmi::Instr& instr) {
        std::string curr_instr;
        auto append = [this, &curr_instr](auto& self, const auto& x) {
            auto unary_instr = [&](const char* iname, const auto& val) {
                back_insert(curr_instr, iname, ' ');
                self(self, val.reg);
            };
            auto bin_instr = [&](const char* iname, const auto& val) {
                back_insert(curr_instr, iname, ' ');
                self(self, val.left);
                back_insert(curr_instr, ", ");
                self(self, val.right);
            };
            using XT = std::decay_t<decltype(x)>;
            if constexpr (std::is_same_v<XT, ir::Label>) {
                back_insert(curr_instr, to_str(x));
            } else if constexpr (std::is_same_v<XT, RegVal>) {
                back_insert(curr_instr, to_asm(x.reg, x.type));
            } else if constexpr (std::is_same_v<XT, VarLocCode>) {
                back_insert(curr_instr, x.code);
            } else if constexpr (std::is_same_v<XT, asmi::mov>) {
                std::visit(
                    [&](const auto& mov) {
                        if constexpr (std::is_same_v<decltype(mov.first), MemLocCode>) {
                            for (auto const& instr_wrp_ptr : mov.first.preparing_instructions)
                            {
                                emit_instr(instr_wrp_ptr->instr);
                            }
                        }
                        if constexpr (std::is_same_v<decltype(mov.second), MemLocCode>) {
                            for (auto const& instr_wrp_ptr : mov.second.preparing_instructions)
                            {
                                emit_instr(instr_wrp_ptr->instr);
                            }
                        }
                        back_insert(curr_instr, "mov ");
                        if constexpr (std::is_same_v<decltype(mov.first), MemLocCode>) {
                            self(self, mov.first.code);
                        } else {
                            self(self, mov.first);
                        }
                        back_insert(curr_instr, ", ");
                        if constexpr (std::is_same_v<decltype(mov.second), MemLocCode>) {
                            self(self, mov.second.code);
                        } else {
                            self(self, mov.second);
                        }
                    },
                    x.val);
            } else if constexpr (std::is_same_v<XT, asmi::lea>) {
                back_insert(curr_instr, "lea ");
                self(self, x.reg);
                back_insert(curr_instr, ", [");
                std::visit(
                    overloaded{
                        [&](const ir::StringConstantName& scn) {
                            back_insert(curr_instr, "rel ", to_str(scn));
                        },
                        [&](const ir::VTableName& vtn) {
                            back_insert(curr_instr, "rel ", to_str(vtn));
                        },
                    },
                    x.val);
                back_insert(curr_instr, ']');
            } else if constexpr (std::is_same_v<XT, asmi::neg>) {
                unary_instr("neg", x);
            } else if constexpr (std::is_same_v<XT, asmi::xor_>) {
                bin_instr("xor", x);
            } else if constexpr (std::is_same_v<XT, asmi::add>) {
                bin_instr("add", x);
            } else if constexpr (std::is_same_v<XT, asmi::sub>) {
                bin_instr("sub", x);
            } else if constexpr (std::is_same_v<XT, asmi::imul>) {
                bin_instr("imul", x);
            } else if constexpr (std::is_same_v<XT, asmi::cdq>) {
                back_insert(curr_instr, "cdq");
            } else if constexpr (std::is_same_v<XT, asmi::idiv>) {
                unary_instr("idiv", x);
            } else if constexpr (std::is_same_v<XT, asmi::test>) {
                bin_instr("test", x);
            } else if constexpr (std::is_same_v<XT, asmi::cmp>) {
                bin_instr("cmp", x);
            } else if constexpr (std::is_same_v<XT, asmi::jmp>) {
                back_insert(curr_instr, "jmp ");
                std::visit([&](const auto& label) { self(self, label); }, x.val);
            } else if constexpr (std::is_same_v<XT, asmi::jz>) {
                back_insert(curr_instr, "jz ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::jnz>) {
                back_insert(curr_instr, "jnz ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::jl>) {
                back_insert(curr_instr, "jl ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::jle>) {
                back_insert(curr_instr, "jle ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::jg>) {
                back_insert(curr_instr, "jg ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::jge>) {
                back_insert(curr_instr, "jge ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::je>) {
                back_insert(curr_instr, "je ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::jne>) {
                back_insert(curr_instr, "jne ");
                self(self, x.target);
            } else if constexpr (std::is_same_v<XT, asmi::push>) {
                back_insert(
                    curr_instr, "push ", to_asm(x.reg.reg, ir::Type::PTR)); // push as 8 bytes
            } else if constexpr (std::is_same_v<XT, asmi::add_rsp>) {
                if (x.how_much != 0) {
                    back_insert(curr_instr, "add rsp, ", x.how_much);
                }
            } else if constexpr (std::is_same_v<XT, asmi::call>) {
                std::visit(
                    overloaded{
                        [&](const std::tuple<const ir::FnName&>& tp) {
                            auto const& [fname] = tp;
                            back_insert(curr_instr, "call ", to_str(fname));
                        },
                        [&](const std::tuple<MemLocCode>& tp) {
                            auto const& [mlc] = tp;
                            for (auto const& instr_wrp_ptr : mlc.preparing_instructions) {
                                emit_instr(instr_wrp_ptr->instr);
                            }
                            back_insert(curr_instr, "call ", mlc.code);
                        },
                    },
                    x.val);
            } else {
                back_insert(curr_instr, x);
            }
        };
        std::visit([&](const auto& i) { append(append, i); }, instr);
        if (not curr_instr.empty()) {
            back_insert(code, '\t', curr_instr, '\n');
        }
    }

    void emit(ir::Instruction& instr) {
        {
            std::ostringstream stream;
            stream << instr;
            back_insert(code, "\t; ", stream.str(), '\n');
        }
        auto emit_call = [this](
                             const std::variant<ir::FnName, ir::ConstMemLoc>& func,
                             const std::vector<ir::Value>& args) {
            for (auto const& arg : reverse_view(args)) {
                auto reg = some_reg(type_of(arg));
                emit_instr(set_to(reg, arg));
                emit_instr(asmi::push{reg});
            }
            std::visit(
                overloaded{
                    [&](const ir::FnName& fname) { emit_instr(asmi::call{fname}); },
                    [&](const ir::ConstMemLoc& mloc) {
                        emit_instr(asmi::call{mloc_code(mloc, ir::Type::PTR)});
                    },
                },
                func);
            emit_instr(asmi::add_rsp{args.size() * 8});
        };
        std::visit(
            overloaded{
                [&](ir::ICopy& i) {
                    if (ir::Value{i.var} != i.val) {
                        auto reg = some_reg(i.var.type);
                        emit_instr(set_to(reg, i.val));
                        emit_instr(asmi::mov{var_loc_code(i.var), reg});
                    }
                },
                [&](ir::IUnaryOp& i) {
                    auto reg = some_reg(i.var.type);
                    emit_instr(set_to(reg, i.val));
                    switch (i.op) {
                    case ir::UnaryOp::NEG: emit_instr(asmi::neg{reg}); break;
                    case ir::UnaryOp::NOT: emit_instr(asmi::xor_{reg, 1}); break;
                    }
                    emit_instr(asmi::mov{var_loc_code(i.var), reg});
                },
                [&](ir::IBinOp& i) {
                    auto rdx = specific_reg(i.var.type, RDX);
                    auto lreg = specific_reg(i.var.type, RAX);
                    assert(i.var.type == type_of(i.left));
                    emit_instr(set_to(lreg, i.left));
                    auto rreg = some_reg(i.var.type);
                    assert(i.var.type == type_of(i.right));
                    emit_instr(set_to(rreg, i.right));
                    switch (i.op) {
                    case ir::BinOp::ADD: emit_instr(asmi::add{lreg, rreg}); break;
                    case ir::BinOp::SUB: emit_instr(asmi::sub{lreg, rreg}); break;
                    case ir::BinOp::MUL: emit_instr(asmi::imul{lreg, rreg}); break;
                    case ir::BinOp::DIV: {
                        assert(lreg.reg == RAX);
                        emit_instr(asmi::cdq{});
                        emit_instr(asmi::idiv{rreg});
                    } break;
                    case ir::BinOp::MOD: {
                        assert(lreg.reg == RAX);
                        emit_instr(asmi::cdq{});
                        emit_instr(asmi::idiv{rreg});
                        lreg = std::move(rdx);
                    } break;
                    }
                    emit_instr(asmi::mov{var_loc_code(i.var), lreg});
                },
                [&](ir::ILoad& i) {
                    auto reg = some_reg(i.var.type);
                    emit_instr(asmi::mov{reg, mloc_code(i.loc, i.var.type)});
                    emit_instr(asmi::mov{var_loc_code(i.var), reg});
                },
                [&](ir::IConstLoad& i) {
                    auto reg = some_reg(i.var.type);
                    emit_instr(asmi::mov{reg, mloc_code(i.loc, i.var.type)});
                    emit_instr(asmi::mov{var_loc_code(i.var), reg});
                },
                [&](ir::IStore& i) {
                    auto val_type = type_of(i.val);
                    auto reg = some_reg(val_type);
                    emit_instr(set_to(reg, i.val));
                    emit_instr(asmi::mov{mloc_code(i.loc, val_type), reg});
                },
                [&](ir::ICall& i) {
                    auto rax = specific_reg(i.var.type, RAX);
                    emit_call(i.func, i.args);
                    emit_instr(asmi::mov{var_loc_code(i.var), rax});
                },
                [&](ir::IVCall& i) { emit_call(i.func, i.args); },
                [&](ir::IGoto& i) {
                    if (i.target != next_bblock_name) {
                        emit_instr(asmi::jmp{i.target});
                    }
                },
                [&](ir::IIfUnaryCond& i) {
                    auto reg = some_reg(ir::Type::BOOL);
                    emit_instr(asmi::mov{reg, var_loc_code(i.cond)});
                    emit_instr(asmi::test{reg, reg});
                    if (i.negate_cond) {
                        emit_instr(asmi::jz{i.true_branch});
                    } else {
                        emit_instr(asmi::jnz{i.true_branch});
                    }
                    if (i.false_branch != next_bblock_name) {
                        emit_instr(asmi::jmp{i.false_branch});
                    }
                },
                [&](ir::IIfBinCond& i) {
                    auto ltype = type_of(i.left);
                    auto rtype = type_of(i.right);
                    assert(ltype == rtype);

                    auto lreg = some_reg(ltype);
                    auto rreg = some_reg(rtype);
                    emit_instr(set_to(lreg, i.left));
                    emit_instr(set_to(rreg, i.right));
                    emit_instr(asmi::cmp{lreg, rreg});
                    switch (ltype) {
                    case ir::Type::INT: {
                        switch (i.op) {
                        case ir::RelOp::LTH: emit_instr(asmi::jl{i.true_branch}); break;
                        case ir::RelOp::LE: emit_instr(asmi::jle{i.true_branch}); break;
                        case ir::RelOp::GTH: emit_instr(asmi::jg{i.true_branch}); break;
                        case ir::RelOp::GE: emit_instr(asmi::jge{i.true_branch}); break;
                        case ir::RelOp::EQ: emit_instr(asmi::je{i.true_branch}); break;
                        case ir::RelOp::NE: emit_instr(asmi::jne{i.true_branch}); break;
                        }
                    } break;
                    case ir::Type::BOOL:
                    case ir::Type::PTR: {
                        switch (i.op) {
                        case ir::RelOp::LTH:
                        case ir::RelOp::LE:
                        case ir::RelOp::GTH:
                        case ir::RelOp::GE: std::abort();
                        case ir::RelOp::EQ: emit_instr(asmi::je{i.true_branch}); break;
                        case ir::RelOp::NE: emit_instr(asmi::jne{i.true_branch}); break;
                        }
                    } break;
                    }
                    if (i.false_branch != next_bblock_name) {
                        emit_instr(asmi::jmp{i.false_branch});
                    }
                },
                [&](ir::IReturn& i) {
                    if (i.val) {
                        auto rax = specific_reg(type_of(*i.val), RAX);
                        emit_instr(set_to(rax, *i.val));
                    }
                    if (next_bblock_name != std::nullopt) {
                        emit_instr(asmi::jmp{epilogue_label});
                    }
                },
                [&](ir::IUnreachable& /*unused*/) {},
            },
            instr);
    }

    void emit(ir::BasicBlock& bblock) {
        back_insert(code, to_str(bblock.name), ":\n");
        assert(bblock.phis.empty());
        for (auto& instr : bblock.instructions) {
            emit(instr);
        }
    }

public:
    SimpleFnEmitter() = default;

    void emit(std::ostream& out, ir::FnDef&& fn) && {
        fdef = &fn;
        {
            size_t i = 0;
            for (auto& arg : fn.args) {
                assert(arg.name.id == i);
                ++i;
            }
        }
        for (size_t i = 0; i < fn.body.size(); ++i) {
            if (i + 1 < fn.body.size()) {
                next_bblock_name = fn.body[i + 1].name;
            } else {
                next_bblock_name = std::nullopt;
            }
            emit(fn.body[i]);
        }
        out << to_str(fn.name) << ": ; (";
        {
            bool first = true;
            for (auto const& arg : fn.args) {
                if (first) {
                    first = false;
                } else {
                    out << ", ";
                }
                out << to_str(arg.type);
            }
        }
        out << ')';
        if (fn.ret_type) {
            out << " -> " << to_str(*fn.ret_type);
        }
        out << '\n';
        out << "\tpush rbp\n";
        out << "\tmov rbp, rsp\n";
        if (stack_size_needed > 0) {
            out << "\tsub rsp, " << stack_size_needed << "\n";
        }
        regs_used.erase(RAX);
        for (auto reg : regs_used) {
            out << "\tpush " << to_asm(reg, ir::Type::PTR) << '\n';
        }

        out << code;

        out << epilogue_label << ":\n";
        for (auto reg : reverse_view(regs_used)) {
            out << "\tpop " << to_asm(reg, ir::Type::PTR) << '\n';
        }
        if (stack_size_needed > 0) {
            out << "\tadd rsp, " << stack_size_needed << "\n";
        }
        out << "\tpop rbp\n";
        out << "\tret\n";
    }
};

class Emitter {
    std::ostream& out;
    const bool disable_destructors;

    void emit(ir::StringConstant& sc) {
        out << sc.name << ": db `";
        for (unsigned char c : sc.value) {
            if (c == '`') {
                out << "\\`";
            } else if (c == '\\') {
                out << "\\\\";
            } else if (isprint(c)) {
                out << c;
            } else {
                auto to_hex = [](int x) -> unsigned char {
                    return x < 10 ? x + '0' : x - 10 + 'a';
                };
                out << "\\x" << to_hex(c >> 4) << to_hex(c & 15);
            }
        }
        out << "\\0";
        out << "`\n";
    }

    void emit(ir::VTable& vtable) {
        out << vtable.name << ":\n";
        for (auto& method : vtable.methods) {
            out << "\tdq " << method << '\n';
        }
    }

    void emit_bultins() {
        out << "section .text\n";
        out << "\n";
        out << "extern abort\n";
        out << "extern calloc\n";
        out << "extern free\n";
        out << "extern malloc\n";
        out << "extern printf\n";
        out << "extern puts\n";
        out << "extern strcmp\n";
        out << "extern strcpy\n";
        out << "extern strlen\n";
        out << "extern scanf\n";
        out << "extern memmove\n";
        out << "extern getline\n";
        out << "extern stdin\n";

        auto emit_label = [this](const char* asm_label) { out << asm_label << ":\n"; };
        auto emit_instr = [this](auto&&... asm_instr) {
            ((out << '\t') << ... << std::forward<decltype(asm_instr)>(asm_instr)) << '\n';
        };

        out << "\nglobal main\nmain:\n";
        {
            emit_instr("jmp ", ir::FnName{.mangled_name = "main"});
        }

        constexpr static std::array regs_to_save = {
            "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11",
        };
        auto emit_builtin_prologue = [&] {
            emit_instr("push rbp");
            emit_instr("mov rbp, rsp");
            for (auto* reg : regs_to_save) {
                emit_instr("push ", reg);
            }
            emit_instr("and rsp, ", ~static_cast<uint64_t>(8)); // align rsp to 16
        };
        auto emit_builtin_epilogue = [&] {
            // restore rsp from before aligning to 16
            static_assert(regs_to_save.size() % 2 == 0);
            emit_instr("and rbp, 8");
            emit_instr("or rsp, rbp");
            for (auto* reg : reverse_view(regs_to_save)) {
                emit_instr("pop ", reg);
            }
            emit_instr("pop rbp");
            emit_instr("ret");
        };
        auto emit_builtin_func = [&](const ir::FnName& name, auto&& body_emitter) {
            out << '\n' << name << ":\n";
            emit_builtin_prologue();
            body_emitter();
            emit_builtin_epilogue();
        };
        auto arg = [&](unsigned num) { return concat("[rbp + ", 16 + 8 * num, ']'); };

        emit_builtin_func(ir::builtin_zalloc, [&] {
            emit_instr("mov edi, 1");
            emit_instr("mov esi, ", arg(0));
            emit_instr("call calloc wrt ..plt");
        });
        if (not disable_destructors) {
            emit_builtin_func(ir::builtin_free, [&] {
                emit_instr("mov rdi, ", arg(0));
                emit_instr("call free wrt ..plt");
            });
        }
        emit_builtin_func(ir::builtin_make_string, [&] {
            emit_instr("mov rdi, ", arg(0));
            emit_instr("call strlen wrt ..plt");
            emit_instr("lea rdi, [rax + 9] ; + 8 for ref count + 1 for trailing null");
            emit_instr("call malloc wrt ..plt");
            emit_instr("mov dword [rax], 1 ; ref count");
            emit_instr("lea rdi, [rax + 8]");
            emit_instr("mov rsi, ", arg(0));
            emit_instr("call strcpy wrt ..plt");
            emit_instr("sub rax, 8");
        });
        emit_builtin_func(ir::builtin_concat_strings, [&] {
            emit_instr("mov rdi, ", arg(0));
            emit_instr("add rdi, 8");
            emit_instr("call strlen wrt ..plt");
            emit_instr("push rax ; first str len");
            emit_instr("sub rsp, 8 ; align stack to 16");

            emit_instr("mov rdi, ", arg(1));
            emit_instr("add rdi, 8");
            emit_instr("call strlen wrt ..plt");
            emit_instr(
                "lea rdi, [rax + 9] ; second str len + 8 for ref count + 1 for trailing null");
            emit_instr("add rdi, [rsp + 8] ; add first str len");
            emit_instr("call malloc wrt ..plt");
            emit_instr("mov dword [rax], 1 ; ref count");
            emit_instr("mov [rsp], rax ; save ptr");
            emit_instr("; copy first string");
            emit_instr("lea rdi, [rax + 8]");
            emit_instr("mov rsi, ", arg(0));
            emit_instr("add rsi, 8");
            emit_instr("call strcpy wrt ..plt");
            emit_instr("; copy second string");
            emit_instr("mov rdi, rax");
            emit_instr("add rdi, [rsp + 8] ; add first str len");
            emit_instr("mov rsi, ", arg(1));
            emit_instr("add rsi, 8");
            emit_instr("call strcpy wrt ..plt");
            emit_instr("pop rax ; restore ptr");
            emit_instr("add rsp, 8 ; restore original rsp");
        });

        auto emit_builtin_strcmp = [&](const ir::FnName& name) {
            emit_builtin_func(name, [&] {
                emit_instr("mov rdi, ", arg(0));
                emit_instr("add rdi, 8");
                emit_instr("mov rsi, ", arg(1));
                emit_instr("add rsi, 8");
                emit_instr("call strcmp wrt ..plt");
                emit_instr("test eax, eax");
                if (ends_with(name.mangled_name, "_lth")) {
                    emit_instr("setl al");
                } else if (ends_with(name.mangled_name, "_le")) {
                    emit_instr("setle al");
                } else if (ends_with(name.mangled_name, "_gth")) {
                    emit_instr("setg al");
                } else if (ends_with(name.mangled_name, "_ge")) {
                    emit_instr("setge al");
                } else if (ends_with(name.mangled_name, "_eq")) {
                    emit_instr("sete al");
                } else if (ends_with(name.mangled_name, "_ne")) {
                    emit_instr("setne al");
                } else {
                    std::abort(); // bug
                }
            });
        };
        emit_builtin_strcmp(ir::builtin_strcmp_lth);
        emit_builtin_strcmp(ir::builtin_strcmp_le);
        emit_builtin_strcmp(ir::builtin_strcmp_gth);
        emit_builtin_strcmp(ir::builtin_strcmp_ge);
        emit_builtin_strcmp(ir::builtin_strcmp_eq);
        emit_builtin_strcmp(ir::builtin_strcmp_ne);

        if (not disable_destructors) {
            emit_builtin_func(ir::builtin_destruct_string, [&] {
                emit_instr("mov rdi, ", arg(0));
                emit_instr(
                    "; no need to test if rdi != 0 because string reference cannot be null");
                emit_instr("sub dword [rdi], 1 ; ref count");
                emit_instr("jnz .L0");
                emit_instr("call free wrt ..plt");
                emit_label(".L0");
            });
        }

        out << '\n' << ir::builtin_inc_ref_count << ":\n";
        {
            emit_instr("mov rax, [rsp + 8]");
            emit_instr("test rax, rax");
            emit_instr("jz .L0");
            emit_instr("add dword [rax], 1");
            emit_label(".L0");
            emit_instr("ret");
        }

        out << '\n' << ir::builtin_error << ":\n";
        {
            emit_instr("and rsp, ", ~static_cast<uint64_t>(8)); // align rsp to 16
            emit_instr("call abort wrt ..plt");
        }

        out << "\nsection .data\n";
        out << "print_int_fmt: db `%i\\n\\0`\n";
        out << "read_int_fmt: db `%i\\n\\0`\n";
        // out << "empty_string: db 0\n";
        out << "section .text\n";

        emit_builtin_func(ir::builtin_printInt, [&] {
            emit_instr("mov esi, ", arg(0));
            emit_instr("lea rdi, [rel print_int_fmt]");
            emit_instr("xor eax, eax");
            emit_instr("call printf wrt ..plt");
        });
        emit_builtin_func(ir::builtin_printString, [&] {
            emit_instr("mov rdi, ", arg(0));
            emit_instr("add rdi, 8");
            emit_instr("call puts wrt ..plt");
            if (not disable_destructors) {
                emit_instr("; destruct argument");
                emit_instr("mov rax, ", arg(0));
                emit_instr("push rax");
                emit_instr("call ", to_str(ir::builtin_destruct_string));
                emit_instr("add rsp, 8");
            }
        });
        emit_builtin_func(ir::builtin_readInt, [&] {
            emit_instr("sub rsp, 16");
            emit_instr("mov dword [rsp], 0 ; in case the scanf fails");
            emit_instr("mov rsi, rsp");
            emit_instr("lea rdi, [rel read_int_fmt]");
            emit_instr("xor eax, eax");
            emit_instr("call scanf wrt ..plt");
            emit_instr("mov eax, [rsp]");
            emit_instr("add rsp, 16");
        });

        emit_builtin_func(ir::builtin_readString, [&] {
            emit_instr("sub rsp, 16");
            emit_instr("mov qword [rsp + 8], 0 ; n");
            emit_instr("mov qword [rsp], 0 ; line");
            emit_instr("mov rdi, rsp ; &line");
            emit_instr("lea rsi, [rsp + 8] ; &n");
            emit_instr("mov rdx, [rel stdin]");
            emit_instr("call getline wrt ..plt");
            emit_instr("mov rdx, [rsp] ; line");
            emit_instr("test rax, rax");
            emit_instr("jl .getline_failed");
            emit_instr("jz .done_removing_newline");
            emit_instr("; getline() > 0");
            emit_instr("mov sil, [rdx + rax - 1] ; last, non-null character");
            emit_instr("cmp sil, 0x0a");
            emit_instr("jne .done_removing_newline");
            emit_instr("mov byte [rdx + rax - 1], 0");
            emit_instr("sub rax, 1");
            emit_label(".done_removing_newline");
            emit_instr("lea rdi, [rax + 9] ; + 8 for ref count + 1 for trailing null");
            emit_instr("cmp rdi, [rsp + 8]");
            emit_instr("jle .no_need_for_malloc");
            emit_instr("call malloc wrt ..plt");
            emit_instr("lea rdi, [rax + 8]");
            emit_instr("mov rsi, [rsp] ; line");
            emit_instr("call strcpy wrt ..plt");
            emit_instr("sub rax, 8");
            emit_instr("mov [rsp + 8], rax ; save ptr (in n's location)");
            emit_instr("mov rdi, [rsp] ; line");
            emit_instr("call free wrt ..plt");
            emit_instr("mov rax, [rsp + 8] ; restore ptr");
            emit_instr("jmp .done");
            emit_label(".no_need_for_malloc");
            emit_instr("lea rdi, [rdx + 8]");
            emit_instr("mov rsi, rdx");
            emit_instr("lea rdx, [rax + 1] ; +  for trailing null");
            emit_instr("call memmove wrt ..plt");
            emit_instr("mov rax, [rsp] ; line");
            emit_instr("jmp .done");
            emit_label(".getline_failed");
            emit_instr("mov rdi, [rsp] ; line");
            emit_instr("call free wrt ..plt");
            emit_instr("mov edi, 9");
            emit_instr("call malloc wrt ..plt");
            emit_instr("mov byte [rax + 8], 0 ; terminating null");
            emit_label(".done");
            emit_instr("mov dword [rax], 1 ; ref count");
            emit_instr("add rsp, 16");
        });
    }

    void emit(ir::FnDef&& fn) {
        out << '\n';
        SimpleFnEmitter{}.emit(out, remove_phis(std::move(fn)));
    }

public:
    explicit Emitter(std::ostream& out, bool disable_destructors)
    : out{out}
    , disable_destructors{disable_destructors} {}

    void emit(ir::Program&& prog) && {
        emit_bultins();
        out << "\nsection .data\n\n";
        for (auto& sc : prog.strings) {
            emit(sc);
        }
        for (auto& vtable : prog.vtables) {
            emit(vtable);
        }
        out << "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
               ";;;;";
        out << "\nsection .text\n";
        for (auto& fn : prog.functions) {
            emit(std::move(fn));
        }
    }
};

} // namespace

namespace backend {

void emit_x86_64(ir::Program&& prog, std::ofstream& out, bool disable_destructors) {
    Emitter{out, disable_destructors}.emit(std::move(prog));
}

} // namespace backend
