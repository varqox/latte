#include "src/ir/ast_to_ir.hh"
#include "src/ast/ast.hh"
#include "src/concat.hh"
#include "src/defs.hh"
#include "src/ir/ir.hh"
#include "src/ir/name_allocator.hh"
#include "src/ir/var_allocator.hh"
#include "src/list.hh"
#include "src/member_comparator.hh"
#include "src/overloaded.hh"
#include "src/persistent_map.hh"
#include "src/ranges.hh"

#include <algorithm>
#include <cassert>
#include <optional>
#include <set>
#include <type_traits>
#include <variant>

namespace {

std::string mangled(const ast::Ident& ident) {
    std::string res = ident;
    std::replace(res.begin(), res.end(), '\'', '#');
    return res;
}

ir::FnName fn_name_from(const ast::Ident& fname) {
    return {.mangled_name = [&] {
        auto res = mangled(fname);
        if (ast::GlobalSymbols::builtin_functions.count(fname) != 0) {
            res.insert(0, 1, '@');
        }
        return res;
    }()};
}

ir::ClassName class_name_from(const ast::Ident& class_name) {
    return {.mangled_name = mangled(class_name)};
}

ir::FnName fn_name_from(const ast::Ident& class_name, const ast::Ident& method_name) {
    return {.mangled_name = concat(mangled(class_name), '.', mangled(method_name))};
}

ir::FnName constructor_name(const ast::Ident& class_name) {
    return {.mangled_name = concat(mangled(class_name), ".@constructor")};
}

ir::FnName initializer_name(const ast::Ident& class_name) {
    return {.mangled_name = concat(mangled(class_name), ".@initializer")};
}

ir::FnName destructor_name(const ast::Ident& class_name) {
    return {.mangled_name = concat(mangled(class_name), ".@destructor")};
}

ir::FnName deinitializer_name(const ast::Ident& class_name) {
    return {.mangled_name = concat(mangled(class_name), ".@deinitializer")};
}

struct BBlockContext {
    ir::FnDef fdef;
    ir::VarAllocator var_allocator;
    NameAllocator<ir::Label> label_allocator;

    struct VarInfo {
        ir::Var var;
        List<ir::Instruction> destructor;
    };
    PersistentMap<ast::Ident, VarInfo> var_env{};
    std::vector<std::vector<List<ir::Instruction>>>
        scope_vars_destructors{}; // scope depth => destructors of variables introduced in the
                                  // scope
    std::vector<std::set<ast::Ident>> scope_vars_set{};
    std::vector<std::vector<List<ir::Instruction>>> tmp_vars_destructors;

    BBlockContext() { new_bblock(label_allocator.alloc()); }

    void new_bblock(ir::Label label) {
        assert(
            fdef.body.empty() or
            (not fdef.body.back().instructions.empty() and
             ends_bblock(fdef.body.back().instructions.back())));
        fdef.body.emplace_back(ir::BasicBlock{
            .name = label,
            .phis = {},
            .instructions = {},
        });
    }

    ir::Var alloc_env_var(ast::Ident name, ir::Type type, List<ir::Instruction> destructor) {
        auto var = var_allocator.alloc(type);
        scope_vars_destructors.back().emplace_back(destructor.clone());
        auto [_, inserted] = scope_vars_set.back().emplace(name);
        assert(inserted && "Shadowing within the same scope is impossible");
        var_env = var_env.insert_or_assign(
            std::move(name), VarInfo{.var = var, .destructor = std::move(destructor)});
        return var;
    }

    void save_tmp_var_destructor(List<ir::Instruction> destructor) {
        tmp_vars_destructors.back().emplace_back(std::move(destructor));
    }

    [[nodiscard]] List<ir::Instruction> pop_tmp_var_destructor() {
        auto& destructors = tmp_vars_destructors.back();
        assert(not destructors.empty());
        auto res = std::move(destructors.back());
        destructors.pop_back();
        return res;
    }

    void append_instruction(ir::Instruction instr) {
        auto& instrs = fdef.body.back().instructions;
        if (instrs.empty() or not ends_bblock(instrs.back())) {
            fdef.body.back().instructions.emplace_back(std::move(instr));
        }
    }

    void append_instructions(List<ir::Instruction>&& instrs) {
        for (auto& instr : instrs) {
            append_instruction(std::move(instr));
        }
    }

    void append_instructions(const List<ir::Instruction>& instrs) {
        for (auto const& instr : instrs) {
            append_instruction(instr);
        }
    }
};

class AstTranslator {
    const ast::GlobalSymbols& global_symbols;
    const bool disable_destructors;
    ir::Program translated_prog;

    bool generated_universal_class_destructor = false;
    std::set<ast::Type> types_with_generated_constructors_and_destructors;

    NameAllocator<ir::StringConstantName> string_name_allocator{};
    std::map<std::string, ir::StringConstantName> strings;

    ir::Value make_string_constant(std::string str) {
        auto it = strings.find(str);
        if (it == strings.end()) {
            auto name = string_name_allocator.alloc();
            translated_prog.strings.emplace_back(ir::StringConstant{
                .name = name,
                .value = str,
            });
            it = strings.try_emplace(std::move(str), name).first;
        }
        return it->second;
    }

    void generate_vtable_for(const ast::GlobalSymbols::Class& cl) {
        auto& vtable = translated_prog.vtables.emplace_back(ir::VTable{
            .name = {.its_class = class_name_from(cl.name)},
            .methods = {},
        });
        vtable.methods.resize(cl.methods.size() + 1);
        if (not disable_destructors) {
            vtable.methods[0] = destructor_name(cl.name);
        } else {
            vtable.methods[0] = ir::builtin_error;
        }
        cl.methods.for_each([&vtable](
                                const ast::Ident& method_name,
                                const ast::GlobalSymbols::Class::Method& method) {
            vtable.methods[method.vtable_idx + 1] =
                fn_name_from(method.defined_in_class, method_name);
        });
    }

    [[nodiscard]] int_t class_sizeof(const ast::Ident& class_name) {
        return 8 + // ref count
            8 + // vtable ptr
            global_symbols.classes.at(class_name).fields.size() * 8;
    }

    [[nodiscard]] static ir::MemLoc ref_count_loc(std::variant<ir::Var, ir::Null> self) {
        return {
            .base = self,
            .displacement = 0,
            .scale = 0,
            .index = 0,
        };
    }

    [[nodiscard]] static ir::ConstMemLoc vtable_loc(std::variant<ir::Var, ir::Null> self) {
        return {
            .loc =
                {
                    .base = self,
                    .displacement = 8,
                    .scale = 0,
                    .index = 0,
                },
        };
    }

    [[nodiscard]] static ir::MemLoc field_loc(
        std::variant<ir::Var, ir::Null> self, const ast::GlobalSymbols::Class& cl,
        const ast::Ident& field_name) {
        return {
            .base = self,
            .displacement = 16 + 8 * cl.fields.find(field_name)->abs_idx,
            .scale = 0,
            .index = 0,
        };
    }

    [[nodiscard]] static ir::ConstMemLoc destructor_loc(ir::Var vtable) {
        return {
            .loc =
                {
                    .base = vtable,
                    .displacement = 0,
                    .scale = 0,
                    .index = 0,
                },
        };
    }

    [[nodiscard]] static ir::ConstMemLoc method_loc(
        ir::Var vtable, const ast::GlobalSymbols::Class& cl, const ast::Ident& method_name) {
        return {
            .loc =
                {
                    .base = vtable,
                    .displacement = 8 * (cl.methods.find(method_name)->vtable_idx + 1),
                    .scale = 0,
                    .index = 0,
                },
        };
    }

    void generate_constructor_for(const ast::TopDef::ClassDef& cl) {
        generate_initializer_for(cl);
        generate_func(
            constructor_name(cl.name), global_symbols.classes.at(cl.name).type, std::nullopt,
            {}, [&](BBlockContext& bbc) {
                auto var = bbc.var_allocator.alloc(ir::Type::PTR);
                bbc.append_instruction(ir::ICall{
                    .var = var,
                    .func = ir::builtin_zalloc,
                    .args = {class_sizeof(cl.name)},
                });
                bbc.append_instruction(ir::IStore{
                    .loc = ref_count_loc(var),
                    .val = 1,
                });
                bbc.append_instruction(ir::IStore{
                    .loc = vtable_loc(var).loc,
                    .val = ir::VTableName{.its_class = class_name_from(cl.name)},
                });
                bbc.append_instruction(ir::IVCall{
                    .func = initializer_name(cl.name),
                    .args = {var},
                });
                bbc.append_instruction(ir::IReturn{
                    .val = var,
                });
            });
    }

    void generate_initializer_for(const ast::TopDef::ClassDef& cl) {
        generate_func(
            initializer_name(cl.name), ast::type_void, NonOwnedSelf{}, {},
            [&](BBlockContext& bbc) {
                auto& self = *bbc.var_env.find("self");
                assert(self.destructor.is_empty());
                if (cl.base_class_name) {
                    bbc.append_instruction(ir::IVCall{
                        .func = initializer_name(*cl.base_class_name),
                        .args = {self.var},
                    });
                }
                auto& cl_sym = global_symbols.classes.at(cl.name);
                for (auto& member : cl.members) {
                    std::visit(
                        overloaded{
                            [&](const ast::ClassMemberDef::FieldDecl& field_decl) {
                                for (auto const& fd_item : field_decl.names) {
                                    auto loc = field_loc(self.var, cl_sym, fd_item.name);
                                    construct_inplace(bbc, field_decl.type, loc);
                                }
                            },
                            [&](const ast::ClassMemberDef::Method& /*unused*/) {
                                // Nothing to do
                            },
                        },
                        member.val);
                }
                bbc.append_instruction(ir::IReturn{
                    .val = std::nullopt,
                });
            });
    }

    void generate_destructor_for(const ast::TopDef::ClassDef& cl) {
        if (disable_destructors) {
            return;
        }
        generate_deinitializer_for(cl);
        generate_func(
            destructor_name(cl.name), ast::type_void, NonOwnedSelf{}, {},
            [&](BBlockContext& bbc) {
                auto& self = *bbc.var_env.find("self");
                assert(self.destructor.is_empty());
                auto prev_ref_count = bbc.var_allocator.alloc(ir::Type::INT);
                bbc.append_instruction(ir::ILoad{
                    .var = prev_ref_count,
                    .loc = ref_count_loc(self.var),
                });
                auto ref_count = bbc.var_allocator.alloc(ir::Type::INT);
                bbc.append_instruction(ir::IBinOp{
                    .var = ref_count,
                    .op = ir::BinOp::SUB,
                    .left = prev_ref_count,
                    .right = 1,
                });
                auto non_zero_rc_label = bbc.label_allocator.alloc();
                auto zero_rc_label = bbc.label_allocator.alloc();
                bbc.append_instruction(ir::IIfBinCond{
                    .op = ir::RelOp::EQ,
                    .left = ref_count,
                    .right = 0,
                    .true_branch = zero_rc_label,
                    .false_branch = non_zero_rc_label,
                });
                // Save decreased value
                bbc.new_bblock(non_zero_rc_label);
                bbc.append_instruction(ir::IStore{
                    .loc = ref_count_loc(self.var),
                    .val = ref_count,
                });
                bbc.append_instruction(ir::IReturn{
                    .val = std::nullopt,
                });
                // Deinit + free
                bbc.new_bblock(zero_rc_label);
                bbc.append_instruction(ir::IVCall{
                    .func = deinitializer_name(cl.name),
                    .args = {self.var},
                });
                bbc.append_instruction(ir::IVCall{
                    .func = ir::builtin_free,
                    .args = {self.var},
                });
                bbc.append_instruction(ir::IReturn{
                    .val = std::nullopt,
                });
            });
    }

    void generate_deinitializer_for(const ast::TopDef::ClassDef& cl) {
        assert(not disable_destructors);
        generate_func(
            deinitializer_name(cl.name), ast::type_void, NonOwnedSelf{}, {},
            [&](BBlockContext& bbc) {
                auto& self = *bbc.var_env.find("self");
                assert(self.destructor.is_empty());
                auto& cl_sym = global_symbols.classes.at(cl.name);
                for (auto& member : cl.members) {
                    std::visit(
                        overloaded{
                            [&](const ast::ClassMemberDef::FieldDecl& field_decl) {
                                if (translate(field_decl.type) == ir::Type::PTR) {
                                    for (auto const& fd_item : field_decl.names) {
                                        auto loc = field_loc(self.var, cl_sym, fd_item.name);
                                        inplace_destructor_generator(field_decl.type, loc)(
                                            bbc);
                                    }
                                }
                            },
                            [&](const ast::ClassMemberDef::Method& /*unused*/) {
                                // Nothing to do
                            },
                        },
                        member.val);
                }
                if (cl.base_class_name) {
                    bbc.append_instruction(ir::IVCall{
                        .func = deinitializer_name(*cl.base_class_name),
                        .args = {self.var},
                    });
                }
                bbc.append_instruction(ir::IReturn{
                    .val = std::nullopt,
                });
            });
    }

    void construct_inplace(BBlockContext& bbc, const ast::Type& type, ir::MemLoc loc) {
        std::visit(
            overloaded{
                [&](const ast::Type::TNull& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TInt& /*unused*/) {
                    // Nothing to do, as memory is already zeroed
                },
                [&](const ast::Type::TStr& /*unused*/) {
                    auto var = bbc.var_allocator.alloc(ir::Type::PTR);
                    bbc.append_instruction(ir::ICall{
                        .var = var,
                        .func = ir::builtin_make_string,
                        .args = {make_string_constant("")},
                    });
                    bbc.append_instruction(ir::IStore{
                        .loc = loc,
                        .val = var,
                    });
                },
                [&](const ast::Type::TBool& /*unused*/) {
                    // Nothing to do, as memory is already zeroed
                },
                [&](const ast::Type::TVoid& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TArray& /*unused*/) {
                    // Nothing to do, as memory is already zeroed
                },
                [&](const ast::Type::TClass& /*unused*/) {
                    // Nothing to do, as memory is already zeroed
                },
                [&](const ast::Type::TFun& /*unused*/) {
                    std::abort(); // should not happen
                },
            },
            type.val);
    }

    void default_initialize(BBlockContext& bbc, const ast::Type& type, ir::Var dest) {
        std::visit(
            overloaded{
                [&](const ast::Type::TNull& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TInt& /*unused*/) {
                    assert(dest.type == ir::Type::INT);
                    bbc.append_instruction(ir::ICopy{
                        .var = dest,
                        .val = 0,
                    });
                },
                [&](const ast::Type::TStr& /*unused*/) {
                    assert(dest.type == ir::Type::PTR);
                    bbc.append_instruction(ir::ICall{
                        .var = dest,
                        .func = ir::builtin_make_string,
                        .args =
                            {
                                make_string_constant(""),
                            },
                    });
                },
                [&](const ast::Type::TBool& /*unused*/) {
                    assert(dest.type == ir::Type::BOOL);
                    bbc.append_instruction(ir::ICopy{
                        .var = dest,
                        .val = false,
                    });
                },
                [&](const ast::Type::TVoid& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TArray& /*unused*/) {
                    assert(dest.type == ir::Type::PTR);
                    bbc.append_instruction(ir::ICopy{
                        .var = dest,
                        .val = ir::Null{},
                    });
                },
                [&](const ast::Type::TClass& /*unused*/) {
                    assert(dest.type == ir::Type::PTR);
                    bbc.append_instruction(ir::ICopy{
                        .var = dest,
                        .val = ir::Null{},
                    });
                },
                [&](const ast::Type::TFun& /*unused*/) {
                    std::abort(); // should not happen
                },
            },
            type.val);
    }

    ir::FnName universal_class_destructor_name() {
        assert(not disable_destructors);
        ir::FnName name = {.mangled_name = "@@universal_class_destructor"};
        if (not generated_universal_class_destructor) {
            generated_universal_class_destructor = true;
            generate_func(name, ast::type_void, NonOwnedSelf{}, {}, [](BBlockContext& bbc) {
                auto& self = *bbc.var_env.find("self");
                assert(self.destructor.is_empty());
                auto return_label = bbc.label_allocator.alloc();
                auto not_null_self_label = bbc.label_allocator.alloc();
                bbc.append_instruction(ir::IIfBinCond{
                    .op = ir::RelOp::EQ,
                    .left = self.var,
                    .right = ir::Null{},
                    .true_branch = return_label,
                    .false_branch = not_null_self_label,
                });
                // self != null
                bbc.new_bblock(not_null_self_label);
                auto vtable = bbc.var_allocator.alloc(ir::Type::PTR);
                bbc.append_instruction(ir::IConstLoad{
                    .var = vtable,
                    .loc = vtable_loc(self.var),
                });
                bbc.append_instruction(ir::IVCall{
                    .func = destructor_loc(vtable),
                    .args = {self.var},
                });
                bbc.append_instruction(ir::IGoto{
                    .target = return_label,
                });
                bbc.new_bblock(return_label);
                bbc.append_instruction(ir::IReturn{
                    .val = std::nullopt,
                });
            });
        }
        return name;
    }

    List<ir::Instruction> destructor_for(ir::Var var, const ast::Type& type) {
        if (disable_destructors) {
            return {};
        }
        List<ir::Instruction> destructor;
        std::visit(
            overloaded{
                [&](const ast::Type::TNull& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TInt& /*unused*/) {
                    // Nothing to do
                },
                [&](const ast::Type::TStr& /*unused*/) {
                    assert(var.type == ir::Type::PTR);
                    destructor.append(ir::IVCall{
                        .func = ir::builtin_destruct_string,
                        .args = {var},
                    });
                },
                [&](const ast::Type::TBool& /*unused*/) {
                    // Nothing to do
                },
                [&](const ast::Type::TVoid& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TArray& ta) {
                    assert(var.type == ir::Type::PTR);
                    destructor.append(ir::IVCall{
                        .func = array_constructor_and_destructor_name(*ta.elem_type).second,
                        .args = {var},
                    });
                },
                [&](const ast::Type::TClass& /*unused*/) {
                    assert(var.type == ir::Type::PTR);
                    destructor.append(ir::IVCall{
                        .func = universal_class_destructor_name(),
                        .args = {var},
                    });
                },
                [&](const ast::Type::TFun& /*unused*/) {
                    std::abort(); // should not happen
                },
            },
            type.val);
        return destructor;
    }

    std::function<void(BBlockContext&)>
    inplace_destructor_generator(const ast::Type& type, ir::MemLoc loc) {
        if (disable_destructors) {
            return [](BBlockContext& /*unused*/) {};
        }
        return std::visit(
            overloaded{
                [&](const ast::Type::TNull& /*unused*/)
                    -> std::function<void(BBlockContext&)> {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TInt& /*unused*/) -> std::function<void(BBlockContext&)> {
                    return [](BBlockContext& /*unused*/) {}; // nothing to do
                },
                [&](const ast::Type::TStr& /*unused*/) -> std::function<void(BBlockContext&)> {
                    return [this, type, loc](BBlockContext& bbc) {
                        auto obj = bbc.var_allocator.alloc(ir::Type::PTR);
                        bbc.append_instruction(ir::ILoad{
                            .var = obj,
                            .loc = loc,
                        });
                        bbc.append_instructions(destructor_for(obj, type));
                    };
                },
                [&](const ast::Type::TBool& /*unused*/)
                    -> std::function<void(BBlockContext&)> {
                    return [](BBlockContext& /*unused*/) {}; // nothing to do
                },
                [&](const ast::Type::TVoid& /*unused*/)
                    -> std::function<void(BBlockContext&)> {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TArray& /*unused*/)
                    -> std::function<void(BBlockContext&)> {
                    return [this, type, loc](BBlockContext& bbc) {
                        auto arr = bbc.var_allocator.alloc(ir::Type::PTR);
                        bbc.append_instruction(ir::ILoad{
                            .var = arr,
                            .loc = loc,
                        });
                        bbc.append_instructions(destructor_for(arr, type));
                    };
                },
                [&](const ast::Type::TClass& /*unused*/)
                    -> std::function<void(BBlockContext&)> {
                    return [this, loc, type](BBlockContext& bbc) {
                        auto obj = bbc.var_allocator.alloc(ir::Type::PTR);
                        bbc.append_instruction(ir::ILoad{
                            .var = obj,
                            .loc = loc,
                        });
                        bbc.append_instructions(destructor_for(obj, type));
                    };
                },
                [&](const ast::Type::TFun& /*unused*/) -> std::function<void(BBlockContext&)> {
                    std::abort(); // should not happen
                },
            },
            type.val);
    }

    List<ir::Instruction> owned_val_maker(ir::Value val, const ast::Type& type) const {
        if (disable_destructors) {
            return {};
        }
        List<ir::Instruction> res;
        std::visit(
            overloaded{
                [&](const ast::Type::TNull& /*unused*/) {
                    // Nothing to do
                },
                [&](const ast::Type::TInt& /*unused*/) {
                    // Nothing to do
                },
                [&](const ast::Type::TStr& /*unused*/) {
                    assert(type_of(val) == ir::Type::PTR);
                    res.append(ir::IVCall{
                        .func = ir::builtin_inc_ref_count,
                        .args = {val},
                    });
                },
                [&](const ast::Type::TBool& /*unused*/) {
                    // Nothing to do
                },
                [&](const ast::Type::TVoid& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Type::TArray& /*unused*/) {
                    assert(type_of(val) == ir::Type::PTR);
                    res.append(ir::IVCall{
                        .func = ir::builtin_inc_ref_count,
                        .args = {val},
                    });
                },
                [&](const ast::Type::TClass& /*unused*/) {
                    assert(type_of(val) == ir::Type::PTR);
                    res.append(ir::IVCall{
                        .func = ir::builtin_inc_ref_count,
                        .args = {val},
                    });
                },
                [&](const ast::Type::TFun& /*unused*/) {
                    std::abort(); // should not happen
                },
            },
            type.val);
        return res;
    }

    void add_env_var(BBlockContext& bbc, const ast::Type& type, ast::Ident name, ir::Var var) {
        auto destructor = destructor_for(var, type);
        bbc.scope_vars_destructors.back().emplace_back(destructor.clone());
        auto [_, inserted] = bbc.scope_vars_set.back().emplace(name);
        assert(inserted && "Shadowing within the same scope is impossible");
        bbc.var_env = bbc.var_env.insert_or_assign(
            std::move(name),
            BBlockContext::VarInfo{
                .var = var,
                .destructor = std::move(destructor),
            });
    }

    ir::Var alloc_env_var(BBlockContext& bbc, const ast::Type& type, ast::Ident name) {
        auto var = bbc.var_allocator.alloc(translate(type));
        add_env_var(bbc, type, std::move(name), var);
        return var;
    }

    [[nodiscard]] static ir::MemLoc arr_ref_cnt_loc(std::variant<ir::Var, ir::Null> arr) {
        return {
            .base = arr,
            .displacement = 0,
            .scale = 0,
            .index = 0,
        };
    }

    [[nodiscard]] static ir::ConstMemLoc arr_len_loc(std::variant<ir::Var, ir::Null> arr) {
        return {
            .loc = {
                .base = arr,
                .displacement = 8,
                .scale = 0,
                .index = 0,
            }};
    }

    [[nodiscard]] static ir::MemLoc
    arr_elem_loc(std::variant<ir::Var, ir::Null> arr, std::variant<ir::Var, int_t> index) {
        return {
            .base = arr,
            .displacement = 16,
            .scale = 8,
            .index = index,
        };
    }

    [[nodiscard]] std::pair<ir::FnName, ir::FnName>
    array_constructor_and_destructor_name(const ast::Type& elem_type) {
        auto arr_type = ast::Type{
            .val = ast::Type::TArray{.elem_type = std::make_unique<ast::Type>(elem_type)},
            .sloc = {.line = 0, .column = 0}};
        auto type_to_str = [](auto&& self, const ast::Type& type) -> std::string {
            return std::visit(
                overloaded{
                    [](const ast::Type::TNull& /*unused*/) -> std::string { std::abort(); },
                    [](const ast::Type::TInt& /*unused*/) -> std::string { return "~int"; },
                    [](const ast::Type::TStr& /*unused*/) -> std::string { return "~str"; },
                    [](const ast::Type::TBool& /*unused*/) -> std::string { return "~bool"; },
                    [](const ast::Type::TVoid& /*unused*/) -> std::string { std::abort(); },
                    [&](const ast::Type::TArray& ta) -> std::string {
                        return concat("~A", self(self, *ta.elem_type));
                    },
                    [](const ast::Type::TClass& tc) -> std::string {
                        return concat('~', mangled(tc.name));
                    },
                    [](const ast::Type::TFun& /*unused*/) -> std::string { std::abort(); },
                },
                type.val);
        };
        auto prefix = type_to_str(type_to_str, arr_type);
        ir::FnName ctor_name = {
            .mangled_name = concat(prefix, ".@constructor"),
        };
        ir::FnName dtor_name = {
            .mangled_name = concat(prefix, ".@destructor"),
        };
        if (types_with_generated_constructors_and_destructors.count(arr_type) == 1) {
            return {ctor_name, dtor_name};
        }

        types_with_generated_constructors_and_destructors.emplace(arr_type);
        generate_func(
            ctor_name, arr_type, std::nullopt,
            {ast::FnArg{
                .type = ast::type_int, .name = "len", .sloc = {.line = 0, .column = 0}}},
            [&](BBlockContext& bbc) {
                auto arr_len = bbc.var_env.find("len")->var;
                auto arr_sizeof = bbc.var_allocator.alloc(ir::Type::INT);
                bbc.append_instruction(ir::IBinOp{
                    .var = arr_sizeof,
                    .op = ir::BinOp::MUL,
                    .left = 8,
                    .right = arr_len,
                });
                bbc.append_instruction(ir::IBinOp{
                    .var = arr_sizeof,
                    .op = ir::BinOp::ADD,
                    .left = arr_sizeof,
                    .right = 16, // sizeof(ref count) + sizeof(array length)
                });
                auto arr = bbc.var_allocator.alloc(ir::Type::PTR);
                bbc.append_instruction(ir::ICall{
                    .var = arr,
                    .func = ir::builtin_zalloc,
                    .args = {arr_sizeof},
                });
                bbc.append_instruction(ir::IStore{
                    .loc = arr_ref_cnt_loc(arr),
                    .val = 1,
                });
                bbc.append_instruction(ir::IStore{
                    .loc = arr_len_loc(arr).loc,
                    .val = arr_len,
                });
                if (elem_type == ast::type_str) {
                    auto idx = bbc.var_allocator.alloc(ir::Type::INT);
                    auto body_label = bbc.label_allocator.alloc();
                    auto cond_label = bbc.label_allocator.alloc();
                    auto done_label = bbc.label_allocator.alloc();
                    bbc.append_instruction(ir::ICopy{
                        .var = idx,
                        .val = 0,
                    });
                    bbc.append_instruction(ir::IGoto{
                        .target = cond_label,
                    });
                    // Loop body
                    bbc.new_bblock(body_label);
                    construct_inplace(bbc, elem_type, arr_elem_loc(arr, idx));
                    bbc.append_instruction(ir::IBinOp{
                        .var = idx,
                        .op = ir::BinOp::ADD,
                        .left = idx,
                        .right = 1,
                    });
                    bbc.append_instruction(ir::IGoto{
                        .target = cond_label,
                    });
                    // Loop condition
                    bbc.new_bblock(cond_label);
                    bbc.append_instruction(ir::IIfBinCond{
                        .op = ir::RelOp::LTH,
                        .left = idx,
                        .right = arr_len,
                        .true_branch = body_label,
                        .false_branch = done_label,
                    });
                    bbc.new_bblock(done_label);
                }
                bbc.append_instruction(ir::IReturn{
                    .val = arr,
                });
            });

        if (disable_destructors) {
            return {ctor_name, dtor_name};
        }
        generate_func(dtor_name, ast::type_void, NonOwnedSelf{}, {}, [&](BBlockContext& bbc) {
            auto& arr_self = *bbc.var_env.find("self");
            assert(arr_self.destructor.is_empty());
            auto& arr = arr_self.var;
            auto return_label = bbc.label_allocator.alloc();
            auto not_null_arr_label = bbc.label_allocator.alloc();
            bbc.append_instruction(ir::IIfBinCond{
                .op = ir::RelOp::EQ,
                .left = arr,
                .right = ir::Null{},
                .true_branch = return_label,
                .false_branch = not_null_arr_label,
            });
            // arr != null
            bbc.new_bblock(not_null_arr_label);
            auto prev_ref_count = bbc.var_allocator.alloc(ir::Type::INT);
            bbc.append_instruction(ir::ILoad{
                .var = prev_ref_count,
                .loc = arr_ref_cnt_loc(arr),
            });
            auto ref_count = bbc.var_allocator.alloc(ir::Type::INT);
            bbc.append_instruction(ir::IBinOp{
                .var = ref_count,
                .op = ir::BinOp::SUB,
                .left = prev_ref_count,
                .right = 1,
            });
            auto non_zero_rc_label = bbc.label_allocator.alloc();
            auto zero_rc_label = bbc.label_allocator.alloc();
            bbc.append_instruction(ir::IIfBinCond{
                .op = ir::RelOp::EQ,
                .left = ref_count,
                .right = 0,
                .true_branch = zero_rc_label,
                .false_branch = non_zero_rc_label,
            });
            // Save decreased value
            bbc.new_bblock(non_zero_rc_label);
            bbc.append_instruction(ir::IStore{
                .loc = arr_ref_cnt_loc(arr),
                .val = ref_count,
            });
            bbc.append_instruction(ir::IReturn{
                .val = std::nullopt,
            });
            // Destruct elements + free
            bbc.new_bblock(zero_rc_label);
            if (translate(elem_type) == ir::Type::PTR) {
                auto idx = bbc.var_allocator.alloc(ir::Type::INT);
                auto body_label = bbc.label_allocator.alloc();
                auto cond_label = bbc.label_allocator.alloc();
                auto done_label = bbc.label_allocator.alloc();
                bbc.append_instruction(ir::IConstLoad{
                    .var = idx,
                    .loc = arr_len_loc(arr),
                });
                bbc.append_instruction(ir::IGoto{
                    .target = cond_label,
                });
                // Loop body
                bbc.new_bblock(body_label);
                bbc.append_instruction(ir::IBinOp{
                    .var = idx,
                    .op = ir::BinOp::SUB,
                    .left = idx,
                    .right = 1,
                });
                inplace_destructor_generator(elem_type, arr_elem_loc(arr, idx))(bbc);
                bbc.append_instruction(ir::IGoto{
                    .target = cond_label,
                });
                // Loop condition
                bbc.new_bblock(cond_label);
                bbc.append_instruction(ir::IIfBinCond{
                    .op = ir::RelOp::GTH,
                    .left = idx,
                    .right = 0,
                    .true_branch = body_label,
                    .false_branch = done_label,
                });
                bbc.new_bblock(done_label);
            }
            bbc.append_instruction(ir::IVCall{
                .func = ir::builtin_free,
                .args = {arr},
            });
            bbc.append_instruction(ir::IGoto{
                .target = return_label,
            });
            bbc.new_bblock(return_label);
            bbc.append_instruction(ir::IReturn{
                .val = std::nullopt,
            });
        });
        return {ctor_name, dtor_name};
    }

    static ir::Type translate(const ast::Type& type) noexcept {
        return std::visit(
            overloaded{
                [](const ast::Type::TNull& /*unused*/) { return ir::Type::PTR; },
                [](const ast::Type::TInt& /*unused*/) { return ir::Type::INT; },
                [](const ast::Type::TStr& /*unused*/) { return ir::Type::PTR; },
                [](const ast::Type::TBool& /*unused*/) { return ir::Type::BOOL; },
                [](const ast::Type::TVoid& /*unused*/) -> ir::Type {
                    std::abort(); // should not happen
                },
                [](const ast::Type::TArray& /*unused*/) { return ir::Type::PTR; },
                [](const ast::Type::TClass& /*unused*/) { return ir::Type::PTR; },
                [](const ast::Type::TFun& /*unused*/) -> ir::Type {
                    std::abort(); // should not happen
                },
            },
            type.val);
    }

    struct [[nodiscard]] LValue {
        std::variant<ir::Var, ir::MemLoc> val;
        std::function<void(BBlockContext& bbc)> val_destructor_generator;
        List<ir::Instruction> other_destructors;
    };

    struct [[nodiscard]] RValue {
        std::optional<ir::Value> val;
        bool is_val_owned = true;
        List<ir::Instruction> make_val_owned;
        List<ir::Instruction> owned_val_destructor;
        List<ir::Instruction> other_destructors;

        void ensure_val_is_owned(BBlockContext& bbc) {
            if (not is_val_owned) {
                bbc.append_instructions(std::move(make_val_owned));
                is_val_owned = true;
            }
        }

        List<ir::Instruction> take_destructors() {
            List<ir::Instruction> destructors;
            if (is_val_owned) {
                destructors = merge(std::move(destructors), std::move(owned_val_destructor));
            }
            return merge(std::move(destructors), std::move(other_destructors));
        }
    };

    static std::variant<ir::Var, int_t> val_to_var_or_int(const ir::Value& val) {
        return std::visit(
            overloaded{
                [&](const ir::Var& var) -> std::variant<ir::Var, int_t> { return var; },
                [&](int_t i) -> std::variant<ir::Var, int_t> { return i; },
                [&](bool /*unused*/) -> std::variant<ir::Var, int_t> { std::abort(); },
                [&](const ir::Null& /*unused*/) -> std::variant<ir::Var, int_t> {
                    std::abort();
                },
                [&](const ir::StringConstantName& /*unused*/) -> std::variant<ir::Var, int_t> {
                    std::abort();
                },
                [&](const ir::VTableName& /*unused*/) -> std::variant<ir::Var, int_t> {
                    std::abort();
                },
            },
            val);
    }

    static std::variant<ir::Var, ir::Null> val_to_var_or_null(const ir::Value& val) {
        return std::visit(
            overloaded{
                [&](const ir::Var& var) -> std::variant<ir::Var, ir::Null> { return var; },
                [&](int_t /*unused*/) -> std::variant<ir::Var, ir::Null> { std::abort(); },
                [&](bool /*unused*/) -> std::variant<ir::Var, ir::Null> { std::abort(); },
                [&](const ir::Null& /*unused*/) -> std::variant<ir::Var, ir::Null> {
                    return ir::Null{};
                },
                [&](const ir::StringConstantName& /*unused*/)
                    -> std::variant<ir::Var, ir::Null> { std::abort(); },
                [&](const ir::VTableName& /*unused*/) -> std::variant<ir::Var, ir::Null> {
                    std::abort();
                },
            },
            val);
    }

    LValue translate_to_lvalue(BBlockContext& bbc, const ast::Expr& expr) {
        return std::visit(
            overloaded{
                [&](const ast::Expr::EVar& var) -> LValue {
                    switch (var.kind) {
                    case ast::Expr::EVar::Kind::LOCAL_VAR: {
                        auto& vinfo = *bbc.var_env.find(var.name);
                        return {
                            .val = vinfo.var,
                            .val_destructor_generator =
                                [dtor = std::make_shared<List<ir::Instruction>>(
                                     vinfo.destructor.clone())](BBlockContext& bbc) {
                                    bbc.append_instructions(*dtor);
                                },
                            .other_destructors = {},
                        };
                    }
                    case ast::Expr::EVar::Kind::CLASS_FIELD: {
                        auto loc = field_loc(
                            bbc.var_env.find("self")->var, *var.field_class, var.name);
                        return {
                            .val = loc,
                            .val_destructor_generator =
                                inplace_destructor_generator(expr.type, loc),
                            .other_destructors = {},
                        };
                    }
                    }
                    __builtin_unreachable();
                },
                [&](const ast::Expr::ELitInt& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::ELitBool& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::ESelf& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::ENull& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::ECastedNull& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::ELitStr& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::EArrElem& arr_elem) -> LValue {
                    auto arr = translate_to_rvalue(bbc, *arr_elem.arr);
                    auto idx = translate_to_rvalue(bbc, *arr_elem.index);
                    auto loc = arr_elem_loc(
                        val_to_var_or_null(arr.val.value()),
                        val_to_var_or_int(idx.val.value()));
                    return {
                        .val = loc,
                        .val_destructor_generator =
                            inplace_destructor_generator(expr.type, loc),
                        .other_destructors =
                            merge(idx.take_destructors(), arr.take_destructors()),
                    };
                },
                [&](const ast::Expr::ECallFunc& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::EField& field) -> LValue {
                    switch (field.kind) {
                    case ast::Expr::EField::Kind::CLASS_FIELD: {
                        auto obj = translate_to_rvalue(bbc, *field.object);
                        auto loc = field_loc(
                            val_to_var_or_null(obj.val.value()), *field.field_class,
                            field.field_name);
                        return {
                            .val = loc,
                            .val_destructor_generator =
                                inplace_destructor_generator(expr.type, loc),
                            .other_destructors = obj.take_destructors(),
                        };
                    }
                    case ast::Expr::EField::Kind::ARRAY_LENGTH: {
                        std::abort(); // cannot be an lvalue
                    }
                    }
                    __builtin_unreachable();
                },
                [&](const ast::Expr::ECallMethod& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::ENewArray& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::ENewClass& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::EUnaryOp& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
                [&](const ast::Expr::EBinOp& /*unused*/) -> LValue {
                    std::abort(); // cannot be an lvalue
                },
            },
            expr.val);
    }

    void translate_bexpr(
        BBlockContext& bbc, const ast::Expr& bexpr, ir::Label true_label,
        ir::Label false_label, ir::Label next_label) {
        assert(not bexpr.comptime_val);
        auto use_translate_to_rvalue = [&] {
            auto val = translate_to_rvalue(bbc, bexpr);
            bbc.append_instructions(val.take_destructors());
            if (next_label == false_label) {
                bbc.append_instruction(ir::IIfUnaryCond{
                    .negate_cond = false,
                    .cond = std::get<ir::Var>(val.val.value()),
                    .true_branch = true_label,
                    .false_branch = false_label,
                });
            } else {
                bbc.append_instruction(ir::IIfUnaryCond{
                    .negate_cond = true,
                    .cond = std::get<ir::Var>(val.val.value()),
                    .true_branch = false_label,
                    .false_branch = true_label,
                });
            }
        };
        std::visit(
            overloaded{
                [&](const ast::Expr::EVar& /*unused*/) { use_translate_to_rvalue(); },
                [&](const ast::Expr::ELitInt& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Expr::ELitBool& /*unused*/) {
                    std::abort(); // should not happen as it has comptime value always set
                },
                [&](const ast::Expr::ESelf& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Expr::ENull& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Expr::ECastedNull& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Expr::ELitStr& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Expr::EArrElem& /*unused*/) { use_translate_to_rvalue(); },
                [&](const ast::Expr::ECallFunc& /*unused*/) { use_translate_to_rvalue(); },
                [&](const ast::Expr::EField& /*unused*/) { use_translate_to_rvalue(); },
                [&](const ast::Expr::ECallMethod& /*unused*/) { use_translate_to_rvalue(); },
                [&](const ast::Expr::ENewArray& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Expr::ENewClass& /*unused*/) {
                    std::abort(); // should not happen
                },
                [&](const ast::Expr::EUnaryOp& uop) {
                    switch (uop.op) {
                    case ast::UnaryOp::NEG: {
                        std::abort(); // should not happen
                    } break;
                    case ast::UnaryOp::NOT: {
                        translate_bexpr(bbc, *uop.val, false_label, true_label, next_label);
                    } break;
                    }
                },
                [&](const ast::Expr::EBinOp& bop) {
                    switch (bop.op) {
                    case ast::BinOp::ADD:
                    case ast::BinOp::SUB:
                    case ast::BinOp::MUL:
                    case ast::BinOp::DIV:
                    case ast::BinOp::MOD: std::abort(); // should not happen
                    case ast::BinOp::LTH:
                    case ast::BinOp::LE:
                    case ast::BinOp::GTH:
                    case ast::BinOp::GE:
                    case ast::BinOp::EQ:
                    case ast::BinOp::NE: {
                        if (bop.left->type == ast::type_str) {
                            return use_translate_to_rvalue();
                        }
                        auto op = [&]() -> ir::RelOp {
                            switch (bop.op) {
                            case ast::BinOp::LTH: return ir::RelOp::LTH;
                            case ast::BinOp::LE: return ir::RelOp::LE;
                            case ast::BinOp::GTH: return ir::RelOp::GTH;
                            case ast::BinOp::GE: return ir::RelOp::GE;
                            case ast::BinOp::EQ: return ir::RelOp::EQ;
                            case ast::BinOp::NE: return ir::RelOp::NE;
                            case ast::BinOp::ADD:
                            case ast::BinOp::SUB:
                            case ast::BinOp::MUL:
                            case ast::BinOp::DIV:
                            case ast::BinOp::MOD:
                            case ast::BinOp::AND:
                            case ast::BinOp::OR:
                                std::abort(); // cannot happen (see the enclosing switch
                                              // statement)
                            }
                            __builtin_unreachable();
                        }();
                        auto lval = translate_to_rvalue(bbc, *bop.left);
                        auto rval = translate_to_rvalue(bbc, *bop.right);
                        bbc.append_instructions(rval.take_destructors());
                        bbc.append_instructions(lval.take_destructors());
                        if (next_label == false_label) {
                            bbc.append_instruction(ir::IIfBinCond{
                                .op = op,
                                .left = lval.val.value(),
                                .right = rval.val.value(),
                                .true_branch = true_label,
                                .false_branch = false_label,
                            });
                        } else {
                            bbc.append_instruction(ir::IIfBinCond{
                                .op = negated(op),
                                .left = lval.val.value(),
                                .right = rval.val.value(),
                                .true_branch = false_label,
                                .false_branch = true_label,
                            });
                        }
                    } break;
                    case ast::BinOp::AND: {
                        if (bop.left->comptime_val) {
                            // If left was always false, then whole condition would always be
                            // false
                            assert(std::get<bool>(*bop.left->comptime_val));
                            return translate_bexpr(
                                bbc, *bop.right, true_label, false_label, next_label);
                        }
                        if (bop.right->comptime_val) {
                            auto rval = std::get<bool>(*bop.right->comptime_val);
                            if (!rval) {
                                auto val = translate_to_rvalue(bbc, *bop.left);
                                bbc.append_instructions(val.take_destructors());
                                bbc.append_instruction(ir::IGoto{
                                    .target = false_label,
                                });
                            } else {
                                translate_bexpr(
                                    bbc, *bop.left, true_label, false_label, next_label);
                            }
                            return;
                        }
                        assert(not bop.left->comptime_val and not bop.right->comptime_val);
                        auto mid_label = bbc.label_allocator.alloc();
                        translate_bexpr(bbc, *bop.left, mid_label, false_label, mid_label);
                        bbc.new_bblock(mid_label);
                        translate_bexpr(bbc, *bop.right, true_label, false_label, next_label);
                    } break;
                    case ast::BinOp::OR: {
                        if (bop.left->comptime_val) {
                            // If left was always true, then whole condition would always be
                            // true
                            assert(!std::get<bool>(*bop.left->comptime_val));
                            return translate_bexpr(
                                bbc, *bop.right, true_label, false_label, next_label);
                        }
                        if (bop.right->comptime_val) {
                            auto rval = std::get<bool>(*bop.right->comptime_val);
                            if (rval) {
                                auto val = translate_to_rvalue(bbc, *bop.left);
                                bbc.append_instructions(val.take_destructors());
                                bbc.append_instruction(ir::IGoto{
                                    .target = true_label,
                                });
                            } else {
                                translate_bexpr(
                                    bbc, *bop.left, true_label, false_label, next_label);
                            }
                            return;
                        }
                        assert(not bop.left->comptime_val and not bop.right->comptime_val);
                        auto mid_label = bbc.label_allocator.alloc();
                        translate_bexpr(bbc, *bop.left, true_label, mid_label, mid_label);
                        bbc.new_bblock(mid_label);
                        translate_bexpr(bbc, *bop.right, true_label, false_label, next_label);
                    } break;
                    }
                },
            },
            bexpr.val);
    }

    RValue translate_to_rvalue(BBlockContext& bbc, const ast::Expr& expr) {
        if (expr.comptime_val) {
            return std::visit(
                overloaded{
                    [&](int_t i) {
                        return RValue{
                            .val = i,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = {},
                            .other_destructors = {},
                        };
                    },
                    [&](bool b) {
                        return RValue{
                            .val = b,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = {},
                            .other_destructors = {},
                        };
                    },
                    [&](const std::string& s) {
                        auto var = bbc.var_allocator.alloc(ir::Type::PTR);
                        bbc.append_instruction(ir::ICall{
                            .var = var,
                            .func = ir::builtin_make_string,
                            .args = {make_string_constant(s)},
                        });
                        return RValue{
                            .val = var,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = destructor_for(var, expr.type),
                            .other_destructors = {},
                        };
                    },
                    [&](ast::Null /*unused*/) {
                        return RValue{
                            .val = ir::Null{},
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = {},
                            .other_destructors = {},
                        };
                    },
                },
                *expr.comptime_val);
        }

        auto local_var = [this, &bbc](const ast::Ident& var_name, const ast::Type& var_type) {
            auto& vinfo = *bbc.var_env.find(var_name);
            return RValue{
                .val = vinfo.var,
                .is_val_owned = false,
                .make_val_owned = owned_val_maker(vinfo.var, var_type),
                .owned_val_destructor = vinfo.destructor.clone(),
                .other_destructors = {},
            };
        };
        auto class_field = [this, &bbc](
                               const ast::Ident& field_name, const ast::Type& field_type,
                               const ast::GlobalSymbols::Class& class_sym, RValue&& obj) {
            auto var = bbc.var_allocator.alloc(translate(field_type));
            bbc.append_instruction(ir::ILoad{
                .var = var,
                .loc = field_loc(val_to_var_or_null(obj.val.value()), class_sym, field_name),
            });
            return RValue{
                .val = var,
                .is_val_owned = false,
                .make_val_owned = owned_val_maker(var, field_type),
                .owned_val_destructor = destructor_for(var, field_type),
                .other_destructors = obj.take_destructors(),
            };
        };

        auto use_translate_bexpr = [&] {
            auto true_label = bbc.label_allocator.alloc();
            auto false_label = bbc.label_allocator.alloc();
            translate_bexpr(bbc, expr, true_label, false_label, true_label);
            auto done_label = bbc.label_allocator.alloc();
            auto var = bbc.var_allocator.alloc(ir::Type::BOOL);
            // True branch
            bbc.new_bblock(true_label);
            bbc.append_instruction(ir::ICopy{
                .var = var,
                .val = true,
            });
            bbc.append_instruction(ir::IGoto{
                .target = done_label,
            });
            // False branch
            bbc.new_bblock(false_label);
            bbc.append_instruction(ir::ICopy{
                .var = var,
                .val = false,
            });
            bbc.append_instruction(ir::IGoto{
                .target = done_label,
            });
            bbc.new_bblock(done_label);
            return RValue{
                .val = var,
                .is_val_owned = true,
                .make_val_owned = {},
                .owned_val_destructor = {},
                .other_destructors = {},
            };
        };

        struct MakeFnNameAndArgsRes {
            std::variant<ir::FnName, ir::ConstMemLoc> func;
            std::vector<ir::Value> args;
            List<ir::Instruction> other_destructors;
        };

        auto translate_fcall = [&](const std::vector<ast::Expr>& args,
                                   auto&& make_name_and_args) {
            static_assert(std::is_invocable_r_v<
                          MakeFnNameAndArgsRes, decltype(make_name_and_args),
                          const std::vector<RValue>&>);
            std::vector<RValue> val_args;
            val_args.reserve(args.size());
            for (auto const& arg : args) {
                auto& rval = val_args.emplace_back(translate_to_rvalue(bbc, arg));
                rval.ensure_val_is_owned(bbc);
            }
            MakeFnNameAndArgsRes mres = make_name_and_args(val_args);
            List<ir::Instruction> destructors;
            for (auto& rval : val_args) {
                destructors = merge(std::move(rval.other_destructors), std::move(destructors));
            }
            if (expr.type == ast::type_void) {
                // void function
                bbc.append_instruction(ir::IVCall{
                    .func = mres.func,
                    .args = std::move(mres.args),
                });
                if (auto f = std::get_if<ir::FnName>(&mres.func);
                    f and *f == fn_name_from("error")) {
                    bbc.append_instruction(ir::IUnreachable{});
                } else {
                    bbc.append_instructions(std::move(mres.other_destructors));
                }
                return RValue{
                    .val = std::nullopt,
                    .is_val_owned = false,
                    .make_val_owned = {},
                    .owned_val_destructor = {},
                    .other_destructors = std::move(destructors),
                };
            }
            // non-void function
            auto var = bbc.var_allocator.alloc(translate(expr.type));
            bbc.append_instruction(ir::ICall{
                .var = var,
                .func = mres.func,
                .args = std::move(mres.args),
            });
            bbc.append_instructions(std::move(mres.other_destructors));
            return RValue{
                .val = var,
                .is_val_owned = true,
                .make_val_owned = {},
                .owned_val_destructor = destructor_for(var, expr.type),
                .other_destructors = std::move(destructors),
            };
        };

        return std::visit(
            overloaded{
                [&](const ast::Expr::EVar& var) -> RValue {
                    switch (var.kind) {
                    case ast::Expr::EVar::Kind::LOCAL_VAR: {
                        return local_var(var.name, expr.type);
                    }
                    case ast::Expr::EVar::Kind::CLASS_FIELD: {
                        auto obj = local_var("self", var.field_class->type);
                        return class_field(
                            var.name, expr.type, *var.field_class, std::move(obj));
                    }
                    }
                    __builtin_unreachable();
                },
                [&](const ast::Expr::ELitInt& /*unused*/) -> RValue {
                    std::abort(); // should not happen as it has comptime value always set
                },
                [&](const ast::Expr::ELitBool& /*unused*/) -> RValue {
                    std::abort(); // should not happen as it has comptime value always set
                },
                [&](const ast::Expr::ESelf& /*unused*/) -> RValue {
                    return local_var("self", expr.type);
                },
                [&](const ast::Expr::ENull& /*unused*/) -> RValue {
                    std::abort(); // should not happen as it has comptime value always set
                },
                [&](const ast::Expr::ECastedNull& /*unused*/) -> RValue {
                    std::abort(); // should not happen as it has comptime value always set
                },
                [&](const ast::Expr::ELitStr& /*unused*/) -> RValue {
                    std::abort(); // should not happen as it has comptime value always set
                },
                [&](const ast::Expr::EArrElem& arr_elem) -> RValue {
                    auto arr = translate_to_rvalue(bbc, *arr_elem.arr);
                    auto idx = translate_to_rvalue(bbc, *arr_elem.index);
                    auto var = bbc.var_allocator.alloc(translate(expr.type));
                    bbc.append_instruction(ir::ILoad{
                        .var = var,
                        .loc = arr_elem_loc(
                            val_to_var_or_null(arr.val.value()),
                            val_to_var_or_int(idx.val.value())),
                    });
                    return {
                        .val = var,
                        .is_val_owned = false,
                        .make_val_owned = owned_val_maker(var, expr.type),
                        .owned_val_destructor = destructor_for(var, expr.type),
                        .other_destructors =
                            merge(idx.take_destructors(), arr.take_destructors()),
                    };
                },
                [&](const ast::Expr::ECallFunc& fcall) -> RValue {
                    return translate_fcall(*fcall.args, [&](const std::vector<RValue>& args) {
                        MakeFnNameAndArgsRes res;
                        switch (fcall.kind) {
                        case ast::Expr::ECallFunc::Kind::METHOD: {
                            auto obj = local_var("self", fcall.method_class->type);
                            obj.ensure_val_is_owned(bbc);
                            auto vtable = bbc.var_allocator.alloc(ir::Type::PTR);
                            bbc.append_instruction(ir::IConstLoad{
                                .var = vtable,
                                .loc = vtable_loc(val_to_var_or_null(obj.val.value())),
                            });
                            res.func =
                                method_loc(vtable, *fcall.method_class, fcall.func_name);
                            res.args.emplace_back(obj.val.value());
                            res.other_destructors = std::move(obj.other_destructors);
                        } break;
                        case ast::Expr::ECallFunc::Kind::BUILTIN:
                        case ast::Expr::ECallFunc::Kind::FUNCTION: {
                            res.func = fn_name_from(fcall.func_name);
                        } break;
                        }
                        for (auto const& arg : args) {
                            res.args.emplace_back(arg.val.value());
                        }
                        return res;
                    });
                },
                [&](const ast::Expr::EField& field) -> RValue {
                    switch (field.kind) {
                    case ast::Expr::EField::Kind::CLASS_FIELD: {
                        auto obj = translate_to_rvalue(bbc, *field.object);
                        return class_field(
                            field.field_name, expr.type, *field.field_class, std::move(obj));
                    }
                    case ast::Expr::EField::Kind::ARRAY_LENGTH: {
                        auto arr = translate_to_rvalue(bbc, *field.object);
                        auto var = bbc.var_allocator.alloc(translate(expr.type));
                        bbc.append_instruction(ir::IConstLoad{
                            .var = var,
                            .loc = arr_len_loc(val_to_var_or_null(arr.val.value())),
                        });
                        return {
                            .val = var,
                            .is_val_owned = false,
                            .make_val_owned = owned_val_maker(var, expr.type),
                            .owned_val_destructor = destructor_for(var, expr.type),
                            .other_destructors = arr.take_destructors(),
                        };
                    }
                    }
                    __builtin_unreachable();
                },
                [&](const ast::Expr::ECallMethod& mcall) -> RValue {
                    return translate_fcall(*mcall.args, [&](const std::vector<RValue>& args) {
                        auto obj = translate_to_rvalue(bbc, *mcall.object);
                        obj.ensure_val_is_owned(bbc);
                        MakeFnNameAndArgsRes res;
                        auto vtable = bbc.var_allocator.alloc(ir::Type::PTR);
                        bbc.append_instruction(ir::IConstLoad{
                            .var = vtable,
                            .loc = vtable_loc(val_to_var_or_null(obj.val.value())),
                        });
                        res.func = method_loc(vtable, *mcall.method_class, mcall.method_name);
                        res.args.emplace_back(obj.val.value());
                        for (auto const& arg : args) {
                            res.args.emplace_back(arg.val.value());
                        }
                        res.other_destructors = std::move(obj.other_destructors);
                        return res;
                    });
                },
                [&](const ast::Expr::ENewArray& na) -> RValue {
                    auto len = translate_to_rvalue(bbc, *na.size);
                    auto var = bbc.var_allocator.alloc(ir::Type::PTR);
                    auto [ctor_name, _] = array_constructor_and_destructor_name(na.elem_type);
                    bbc.append_instruction(ir::ICall{
                        .var = var,
                        .func = ctor_name,
                        .args = {len.val.value()},
                    });
                    return {
                        .val = var,
                        .is_val_owned = true,
                        .make_val_owned = {},
                        .owned_val_destructor = destructor_for(var, expr.type),
                        .other_destructors = len.take_destructors(),
                    };
                },
                [&](const ast::Expr::ENewClass& nc) -> RValue {
                    auto var = bbc.var_allocator.alloc(ir::Type::PTR);
                    bbc.append_instruction(ir::ICall{
                        .var = var,
                        .func = constructor_name(nc.class_name),
                        .args = {},
                    });
                    return {
                        .val = var,
                        .is_val_owned = true,
                        .make_val_owned = {},
                        .owned_val_destructor = destructor_for(var, expr.type),
                        .other_destructors = {},
                    };
                },
                [&](const ast::Expr::EUnaryOp& uop) -> RValue {
                    switch (uop.op) {
                    case ast::UnaryOp::NEG: {
                        auto val = translate_to_rvalue(bbc, *uop.val);
                        auto var = bbc.var_allocator.alloc(ir::Type::INT);
                        bbc.append_instruction(ir::IUnaryOp{
                            .var = var,
                            .op = ir::UnaryOp::NEG,
                            .val = val.val.value(),
                        });
                        return {
                            .val = var,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = {},
                            .other_destructors = val.take_destructors(),
                        };
                    }
                    case ast::UnaryOp::NOT: {
                        if (auto e = std::get_if<ast::Expr::EUnaryOp>(&uop.val->val)) {
                            assert(e->op == ast::UnaryOp::NOT);
                            return translate_to_rvalue(bbc, *e->val); // Skip double negation
                        }
                        if (std::holds_alternative<ast::Expr::EBinOp>(uop.val->val)) {
                            return use_translate_bexpr();
                        }
                        auto val = translate_to_rvalue(bbc, *uop.val);
                        auto var = bbc.var_allocator.alloc(ir::Type::BOOL);
                        bbc.append_instruction(ir::IUnaryOp{
                            .var = var,
                            .op = ir::UnaryOp::NOT,
                            .val = val.val.value(),
                        });
                        return {
                            .val = var,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = {},
                            .other_destructors = val.take_destructors(),
                        };
                    }
                    }
                    __builtin_unreachable();
                },
                [&](const ast::Expr::EBinOp& bop) -> RValue {
                    auto translate_int_binop = [&](ir::BinOp op) {
                        auto lval = translate_to_rvalue(bbc, *bop.left);
                        auto rval = translate_to_rvalue(bbc, *bop.right);
                        auto var = bbc.var_allocator.alloc(ir::Type::INT);
                        assert(expr.type == ast::type_int);
                        bbc.append_instruction(ir::IBinOp{
                            .var = var,
                            .op = op,
                            .left = lval.val.value(),
                            .right = rval.val.value(),
                        });
                        return RValue{
                            .val = var,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = {},
                            .other_destructors =
                                merge(rval.take_destructors(), lval.take_destructors()),
                        };
                    };
                    auto use_string_relop = [&](const ir::FnName& strcmp_func) {
                        auto lval = translate_to_rvalue(bbc, *bop.left);
                        auto rval = translate_to_rvalue(bbc, *bop.right);
                        auto var = bbc.var_allocator.alloc(ir::Type::BOOL);
                        bbc.append_instruction(ir::ICall{
                            .var = var,
                            .func = strcmp_func,
                            .args = {lval.val.value(), rval.val.value()},
                        });
                        return RValue{
                            .val = var,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = {},
                            .other_destructors =
                                merge(rval.take_destructors(), lval.take_destructors()),
                        };
                    };
                    switch (bop.op) {
                    case ast::BinOp::ADD: {
                        if (expr.type == ast::type_int) {
                            return translate_int_binop(ir::BinOp::ADD);
                        }
                        assert(expr.type == ast::type_str);
                        auto lval = translate_to_rvalue(bbc, *bop.left);
                        auto rval = translate_to_rvalue(bbc, *bop.right);
                        auto var = bbc.var_allocator.alloc(ir::Type::PTR);
                        bbc.append_instruction(ir::ICall{
                            .var = var,
                            .func = ir::builtin_concat_strings,
                            .args = {lval.val.value(), rval.val.value()},
                        });
                        return {
                            .val = var,
                            .is_val_owned = true,
                            .make_val_owned = {},
                            .owned_val_destructor = destructor_for(var, expr.type),
                            .other_destructors =
                                merge(rval.take_destructors(), lval.take_destructors()),
                        };
                    }
                    case ast::BinOp::SUB: {
                        return translate_int_binop(ir::BinOp::SUB);
                    }
                    case ast::BinOp::MUL: {
                        return translate_int_binop(ir::BinOp::MUL);
                    }
                    case ast::BinOp::DIV: {
                        return translate_int_binop(ir::BinOp::DIV);
                    }
                    case ast::BinOp::MOD: {
                        return translate_int_binop(ir::BinOp::MOD);
                    }
                    case ast::BinOp::LTH: {
                        if (bop.left->type == ast::type_str) {
                            return use_string_relop(ir::builtin_strcmp_lth);
                        }
                        return use_translate_bexpr();
                    }
                    case ast::BinOp::LE: {
                        if (bop.left->type == ast::type_str) {
                            return use_string_relop(ir::builtin_strcmp_le);
                        }
                        return use_translate_bexpr();
                    }
                    case ast::BinOp::GTH: {
                        if (bop.left->type == ast::type_str) {
                            return use_string_relop(ir::builtin_strcmp_gth);
                        }
                        return use_translate_bexpr();
                    }
                    case ast::BinOp::GE: {
                        if (bop.left->type == ast::type_str) {
                            return use_string_relop(ir::builtin_strcmp_ge);
                        }
                        return use_translate_bexpr();
                    }
                    case ast::BinOp::EQ: {
                        if (bop.left->type == ast::type_str) {
                            return use_string_relop(ir::builtin_strcmp_eq);
                        }
                        return use_translate_bexpr();
                    }
                    case ast::BinOp::NE: {
                        if (bop.left->type == ast::type_str) {
                            return use_string_relop(ir::builtin_strcmp_ne);
                        }
                        return use_translate_bexpr();
                    }
                    case ast::BinOp::AND:
                    case ast::BinOp::OR: {
                        return use_translate_bexpr();
                    }
                    }
                    __builtin_unreachable();
                },
            },
            expr.val);
    }

    void
    translate(BBlockContext& bbc, const ast::DeclItem& decl_item, const ast::Type& decl_type) {
        std::visit(
            overloaded{
                [&](const ast::DeclItem::DNoInit& decl) {
                    auto var = alloc_env_var(bbc, decl_type, decl.name);
                    default_initialize(bbc, decl_type, var);
                },
                [&](const ast::DeclItem::DInit& decl) {
                    auto val = translate_to_rvalue(bbc, decl.val);
                    val.ensure_val_is_owned(bbc);
                    auto& val_val = val.val.value();
                    bbc.append_instruction(ir::ICopy{
                        .var = alloc_env_var(bbc, decl_type, decl.name),
                        .val = val_val,
                    });
                    bbc.append_instructions(val.other_destructors);
                },
            },
            decl_item.val);
    }

    template <class Func>
    void in_new_scope(BBlockContext& bbc, Func&& scope_filler) {
        auto old_var_env = bbc.var_env;
        bbc.scope_vars_destructors.emplace_back();
        bbc.scope_vars_set.emplace_back();
        bbc.tmp_vars_destructors.emplace_back();
        scope_filler();
        for (auto& destructor : reverse_view(bbc.scope_vars_destructors.back())) {
            bbc.append_instructions(std::move(destructor));
        }
        assert(bbc.tmp_vars_destructors.back().empty());
        bbc.tmp_vars_destructors.pop_back();
        bbc.scope_vars_destructors.pop_back();
        bbc.scope_vars_set.pop_back();
        bbc.var_env = std::move(old_var_env);
    }

    void translate(BBlockContext& bbc, const ast::Stmt& stmt) {
        if (stmt.reachability == ast::Reachability::UNREACHABLE) {
            return;
        }
        auto do_xxcrement = [&](const ast::Expr& dest_expr, ir::BinOp op) {
            auto dest = translate_to_lvalue(bbc, dest_expr);
            dest.val_destructor_generator(bbc);
            std::visit(
                overloaded{
                    [&](ir::Var& var) {
                        bbc.append_instruction(ir::IBinOp{
                            .var = var,
                            .op = op,
                            .left = var,
                            .right = 1,
                        });
                    },
                    [&](ir::MemLoc& loc) {
                        auto var = bbc.var_allocator.alloc(translate(dest_expr.type));
                        bbc.append_instruction(ir::ILoad{
                            .var = var,
                            .loc = loc,
                        });
                        bbc.append_instruction(ir::IBinOp{
                            .var = var,
                            .op = op,
                            .left = var,
                            .right = 1,
                        });
                        bbc.append_instruction(ir::IStore{
                            .loc = loc,
                            .val = var,
                        });
                    },
                },
                dest.val);
            bbc.append_instructions(std::move(dest.other_destructors));
        };
        auto destruct_everything_before_return = [&] {
            assert(bbc.scope_vars_destructors.size() == bbc.tmp_vars_destructors.size());
            for (size_t scope_depth = bbc.scope_vars_destructors.size(); scope_depth > 0;) {
                --scope_depth;
                for (auto const& destructor :
                     reverse_view(bbc.tmp_vars_destructors[scope_depth])) {
                    bbc.append_instructions(destructor);
                }
                for (auto const& destructor :
                     reverse_view(bbc.scope_vars_destructors[scope_depth])) {
                    bbc.append_instructions(destructor);
                }
            }
        };

        std::visit(
            overloaded{
                [&](const ast::Stmt::SEmpty& /*unused*/) {},
                [&](const ast::Stmt::SBlock& sb) { translate(bbc, *sb.block); },
                [&](const ast::Stmt::SDecl& decl) {
                    for (auto& item : decl.items) {
                        translate(bbc, item, decl.type);
                    }
                },
                [&](const ast::Stmt::SAss& ass) {
                    auto dest = translate_to_lvalue(bbc, ass.dest);
                    auto val = translate_to_rvalue(bbc, ass.val);
                    val.ensure_val_is_owned(bbc);
                    dest.val_destructor_generator(bbc);
                    std::visit(
                        overloaded{
                            [&](ir::Var& var) {
                                bbc.append_instruction(ir::ICopy{
                                    .var = var,
                                    .val = val.val.value(),
                                });
                            },
                            [&](ir::MemLoc& loc) {
                                bbc.append_instruction(ir::IStore{
                                    .loc = loc,
                                    .val = val.val.value(),
                                });
                            },
                        },
                        dest.val);
                    bbc.append_instructions(val.other_destructors);
                    bbc.append_instructions(std::move(dest.other_destructors));
                },
                [&](const ast::Stmt::SIncr& incr) { do_xxcrement(incr.dest, ir::BinOp::ADD); },
                [&](const ast::Stmt::SDecr& decr) { do_xxcrement(decr.dest, ir::BinOp::SUB); },
                [&](const ast::Stmt::SRet& ret) {
                    auto val = translate_to_rvalue(bbc, ret.val);
                    val.ensure_val_is_owned(bbc);
                    bbc.append_instructions(std::move(val.other_destructors));
                    destruct_everything_before_return();
                    bbc.append_instruction(ir::IReturn{
                        .val = val.val.value(),
                    });
                },
                [&](const ast::Stmt::SVRet& /*unused*/) {
                    destruct_everything_before_return();
                    bbc.append_instruction(ir::IReturn{
                        .val = std::nullopt,
                    });
                },
                [&](const ast::Stmt::SExpr& expr) {
                    auto val = translate_to_rvalue(bbc, expr.val);
                    bbc.append_instructions(val.take_destructors());
                },
                [&](const ast::Stmt::SWhile& swhile) {
                    if (swhile.cond.comptime_val) {
                        // false condition renders the whole while dead
                        assert(std::get<bool>(*swhile.cond.comptime_val));
                        auto label = bbc.label_allocator.alloc();
                        bbc.append_instruction(ir::IGoto{
                            .target = label,
                        });
                        bbc.new_bblock(label);
                        translate(bbc, *swhile.body);
                        bbc.append_instruction(ir::IGoto{
                            .target = label,
                        });
                        return;
                    }
                    auto body_label = bbc.label_allocator.alloc();
                    auto cond_label = bbc.label_allocator.alloc();
                    bbc.append_instruction(ir::IGoto{
                        .target = cond_label,
                    });

                    bbc.new_bblock(body_label);
                    translate(bbc, *swhile.body);
                    bbc.append_instruction(ir::IGoto{
                        .target = cond_label,
                    });

                    bbc.new_bblock(cond_label);
                    auto done_label = bbc.label_allocator.alloc();
                    translate_bexpr(bbc, swhile.cond, body_label, done_label, done_label);
                    bbc.new_bblock(done_label);
                },
                [&](const ast::Stmt::SFor& sfor) {
                    auto arr_val = translate_to_rvalue(bbc, sfor.arr);
                    arr_val.ensure_val_is_owned(bbc);
                    auto arr = bbc.var_allocator.alloc(translate(sfor.arr.type));
                    bbc.append_instruction(ir::ICopy{
                        .var = arr,
                        .val = arr_val.val.value(),
                    });
                    bbc.save_tmp_var_destructor(destructor_for(arr, sfor.arr.type));

                    auto arr_len = bbc.var_allocator.alloc(ir::Type::INT);
                    bbc.append_instruction(ir::IConstLoad{
                        .var = arr_len,
                        .loc = arr_len_loc(arr),
                    });
                    auto idx = bbc.var_allocator.alloc(ir::Type::INT);
                    bbc.append_instruction(ir::ICopy{
                        .var = idx,
                        .val = 0,
                    });
                    auto body_label = bbc.label_allocator.alloc();
                    auto cond_label = bbc.label_allocator.alloc();
                    bbc.append_instruction(ir::IGoto{
                        .target = cond_label,
                    });

                    bbc.new_bblock(body_label);
                    in_new_scope(bbc, [&] {
                        auto elem = alloc_env_var(bbc, sfor.iter_type, sfor.iter_name);
                        bbc.append_instruction(ir::ILoad{
                            .var = elem,
                            .loc = arr_elem_loc(arr, idx),
                        });
                        bbc.append_instructions(owned_val_maker(elem, sfor.iter_type));
                        translate(bbc, *sfor.body);
                    });
                    bbc.append_instruction(ir::IBinOp{
                        .var = idx,
                        .op = ir::BinOp::ADD,
                        .left = idx,
                        .right = 1,
                    });
                    bbc.append_instruction(ir::IGoto{
                        .target = cond_label,
                    });

                    bbc.new_bblock(cond_label);
                    auto done_label = bbc.label_allocator.alloc();
                    bbc.append_instruction(ir::IIfBinCond{
                        .op = ir::RelOp::LTH,
                        .left = idx,
                        .right = arr_len,
                        .true_branch = body_label,
                        .false_branch = done_label,
                    });

                    bbc.new_bblock(done_label);
                    bbc.append_instructions(bbc.pop_tmp_var_destructor());
                },
                [&](const ast::Stmt::SIf& sif) {
                    if (sif.cond.comptime_val) {
                        if (std::get<bool>(*sif.cond.comptime_val)) {
                            translate(bbc, *sif.true_branch);
                        } else if (sif.false_branch) {
                            translate(bbc, *sif.false_branch);
                        }
                        return;
                    }

                    auto true_label = bbc.label_allocator.alloc();
                    auto done_label = bbc.label_allocator.alloc();
                    auto false_label =
                        sif.false_branch ? bbc.label_allocator.alloc() : done_label;
                    translate_bexpr(bbc, sif.cond, true_label, false_label, true_label);

                    bbc.new_bblock(true_label);
                    translate(bbc, *sif.true_branch);
                    bbc.append_instruction(ir::IGoto{
                        .target = done_label,
                    });

                    if (sif.false_branch) {
                        bbc.new_bblock(false_label);
                        translate(bbc, *sif.false_branch);
                        bbc.append_instruction(ir::IGoto{
                            .target = done_label,
                        });
                    }

                    bbc.new_bblock(done_label);
                },
            },
            stmt.val);
    }

    void translate(BBlockContext& bbc, const ast::Block& block) {
        if (block.reachability == ast::Reachability::UNREACHABLE) {
            return;
        }
        in_new_scope(bbc, [&] {
            for (auto const& stmt : block.stmts) {
                translate(bbc, stmt);
            }
        });
    }

    struct NonOwnedSelf {};

    template <class BodyGenerator>
    void generate_func(
        ir::FnName fname, const ast::Type& ret_type,
        std::optional<std::variant<ast::Type, NonOwnedSelf>> self_type,
        const std::vector<ast::FnArg>& args, BodyGenerator&& body_generator) {
        static_assert(std::is_invocable_v<BodyGenerator&&, BBlockContext&>);
        BBlockContext bbc;
        bbc.fdef.name = std::move(fname);
        bbc.fdef.ret_type =
            ret_type == ast::type_void ? std::nullopt : std::optional{translate(ret_type)};
        bbc.scope_vars_destructors.emplace_back();
        bbc.scope_vars_set.emplace_back();
        bbc.tmp_vars_destructors.emplace_back();

        bbc.fdef.args.reserve(args.size() + self_type.has_value());
        // Every arg owns its value
        if (self_type) {
            bbc.fdef.args.emplace_back(std::visit(
                overloaded{
                    [&](const ast::Type& type) { return alloc_env_var(bbc, type, "self"); },
                    [&](const NonOwnedSelf& /*unused*/) {
                        return bbc.alloc_env_var("self", ir::Type::PTR, {});
                    },
                },
                self_type.value()));
        }
        for (auto const& arg : args) {
            bbc.fdef.args.emplace_back(alloc_env_var(bbc, arg.type, arg.name));
        }
        body_generator(bbc);
        // Destructors for arguments
        assert(bbc.scope_vars_destructors.size() == 1);
        for (auto const& destructor : reverse_view(bbc.scope_vars_destructors.back())) {
            bbc.append_instructions(destructor);
        }
        if (ret_type == ast::type_void) {
            bbc.append_instruction(ir::IReturn{
                .val = std::nullopt,
            });
        } else {
            bbc.append_instruction(ir::IUnreachable{});
        }
        assert(ends_bblock(bbc.fdef.body.back().instructions.back()));
        translated_prog.functions.emplace_back(std::move(bbc.fdef));
    }

    void translate(const ast::TopDef::ClassDef& cl) {
        generate_vtable_for(global_symbols.classes.at(cl.name));
        generate_constructor_for(cl);
        generate_destructor_for(cl);
        auto const& class_type = global_symbols.classes.at(cl.name).type;
        types_with_generated_constructors_and_destructors.emplace(class_type);
        for (auto& member : cl.members) {
            std::visit(
                overloaded{
                    [&](const ast::ClassMemberDef::FieldDecl& /*unused*/) {
                        // Nothing to translate
                    },
                    [&](const ast::ClassMemberDef::Method& method) {
                        generate_func(
                            fn_name_from(cl.name, method.name), method.ret_type, class_type,
                            method.args,
                            [&](BBlockContext& bbc) { translate(bbc, method.body); });
                    },
                },
                member.val);
        }
    }

public:
    explicit AstTranslator(const ast::GlobalSymbols& global_symbols, bool disable_destructors)
    : global_symbols{global_symbols}
    , disable_destructors{disable_destructors} {}

    ir::Program translate(const ast::Program& prog) && {
        for (auto& top_def : prog.top_defs) {
            std::visit(
                overloaded{
                    [&](const ast::TopDef::FnDef& fn) {
                        generate_func(
                            fn_name_from(fn.name), fn.ret_type, std::nullopt, fn.args,
                            [&](BBlockContext& bbc) { translate(bbc, fn.body); });
                    },
                    [&](const ast::TopDef::ClassDef& cl) { translate(cl); },
                },
                top_def.val);
        }
        return std::move(translated_prog);
    }
};

} // namespace

namespace ir {

Program translate_ast_to_ir(
    const ast::Program& prog, const ast::GlobalSymbols& global_symbols,
    bool disable_destructors) {
    return AstTranslator{global_symbols, disable_destructors}.translate(prog);
}

} // namespace ir
